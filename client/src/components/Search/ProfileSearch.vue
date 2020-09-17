<template>
    <div class="container">
        <slot name="header">
            <h1 class="title">Profile Search</h1>
        </slot>
        <form @submit.prevent="searchUser">
            <b-field label="Name" expanded>
                <b-input type="text"
                         v-model="name"
                         placeholder="Name">
                </b-input>
            </b-field>

            <b-field label="Email" expanded>
                <b-input type="text"
                         v-model="email"
                         placeholder="Email">
                </b-input>
            </b-field>
            <br>

          <ActivityTypesField v-on:updateSearchMethod="newSearchMethod => activitySearchType = newSearchMethod" v-on:updateChosenActivityTypes="newActivityTypes => chosenActivityTypes = newActivityTypes" :chosenActivityTypes="chosenActivityTypes" :activitySearchType="activitySearchType"></ActivityTypesField>

            <br>
            <div class="column">
                <div class="is-pulled-left">
                    <b-button type="is-danger" @click="resetSearchFields">Reset fields</b-button>
                </div>
                <div class="is-pulled-right">
                    <b-field>
                        <b-button native-type="submit" class="is-primary">Search</b-button>
                    </b-field>
                </div>
            </div>

        </form>

        <div id="results" class="column" v-if="profiles.length">
            <div
                    v-for="profile in profiles"
                    :key="profile.id">
                <ProfileSummary :profile="profile" @deleteClicked="deleteProfile">
                    <template #options v-on:viewProfileClicked="openProfileModal">
                        <b-menu-item class="isVerticalCenter">
                            <template slot="label">
                                <b-dropdown aria-role="list" class="is-pulled-right" position="is-bottom-left" v-if="store.getters.getAuthenticationLevel <= 1">
                                    <b-icon icon="ellipsis-v" slot="trigger"></b-icon>
                                    <b-dropdown-item aria-role="listitem" @click="openProfileModal(profile)">View profile</b-dropdown-item>
                                    <b-dropdown-item aria-role="listitem" @click="editProfile(profile)">Edit profile </b-dropdown-item>
                                    <b-dropdown-item aria-role="listitem" @click="onDeleteProfileClicked(profile)">Delete profile</b-dropdown-item>
                                    <b-dropdown-item v-if="profile.authLevel < 2" aria-role="listitem" @click="onChangeAdminRightsClicked(profile,'user')">Remove admin rights</b-dropdown-item>
                                    <b-dropdown-item v-else aria-role="listitem" @click="onChangeAdminRightsClicked(profile,'admin')">Make admin</b-dropdown-item>
                                </b-dropdown>
                                <b-button v-else type="is-text" @click="openProfileModal(profile)">View profile</b-button>
                            </template>
                        </b-menu-item>
                    </template>
                </ProfileSummary>
            </div>
        </div>

        <div v-else id="noMatches">
            <h1>No profiles loaded!</h1>
        </div>

        <observer v-on:intersect="loadMoreProfiles"></observer>
    </div>
</template>

<script>
    import Api from "../../Api";
    import ProfileSummary from "../ProfileSummary";
    import Observer from "../Observer";
    import toastMixin from "../../mixins/toastMixin";
    import {eventBus} from "../../main";
    import NavBar from "../NavBar";
    import store from "../../store";
    import Profile from "../Profile";
    import EditProfile from "../EditProfile/EditProfile";
    import router from "../../router";
    import ActivityTypesField from "@/components/ActivityTypesField";

    const DEFAULT_RESULT_COUNT = 10

    export default {
        name: "ProfileSearch",
        mixins: [toastMixin],
        components: {ActivityTypesField, Observer, ProfileSummary},
        data() {
            return {
                store: store,
                activitySearchType: "all",
                chosenActivityTypes: [],
                email: "",
                name: "",
                observer: null,
                profiles: [],
                startIndex: 0,
                moreProfilesExist: true
            }
        },

        created() {
            //Used to update the list of profiles when a profile on this list is changed
            //This is done by watching for the profileWasEdited event on the global event bus
            eventBus.$on('profileWasEdited', (editedProfile) => {
                for (let i = 0; i < this.profiles.length; i++) {
                    if(this.profiles[i].id === editedProfile.id){
                        this.profiles[i].firstname = editedProfile.firstname
                        this.profiles[i].lastname = editedProfile.lastname
                        this.profiles[i].email = editedProfile.primary_email
                        this.profiles[i].activities = editedProfile.activities
                    }
                }
            })
        },
        methods: {
          openProfileModal(profile) {
                this.$buefy.modal.open({
                    parent: this,
                    props: {id: profile.id},
                    component: Profile,
                    trapFocus: true,
                    scroll: "clip"
                })
            },
            editProfile(profile){
                this.$buefy.modal.open({
                    parent: this,
                    props: {id: profile.id},
                    component: EditProfile,
                    trapFocus: true,
                    scroll: "clip"
                })
            },
            onChangeAdminRightsClicked(profile, permissionLevel){
                this.$buefy.dialog.confirm({
                    message: `Are you sure you want to change ${profile.firstname}'s role to ${permissionLevel}?`,
                    confirmText: 'Yes',
                    onConfirm: () =>  this.changeAdminRights(profile, permissionLevel)
                })
            },
            changeAdminRights(profile, permissionLevel){
                Api.editProfilePermissions(profile.id, permissionLevel, localStorage.getItem("authToken"))
                    .then(() => {
                        if(permissionLevel === "admin"){
                            this.successToast(`${profile.firstname} is now an admin`)
                            profile.authLevel = 1
                        } else {
                            if (profile.id == this.store.getters.getUserId) {
                                this.successToast(`You are no longer an admin`)
                                profile.authLevel = 5
                                this.store.commit("SET_AUTHENTICATION_LEVEL", 5)
                                router.push({path: '/Profile/' + store.getters.getUserId})
                            }
                            else {
                                this.successToast(`${profile.firstname} is no longer an admin`)
                                profile.authLevel = 5
                            }
                        }

                    })
                    .catch(()  => {
                        this.warningToast(`Could not change user to ${permissionLevel}.`);
                    })
            },
            onDeleteProfileClicked(profileToDelete) {
                this.$buefy.dialog.confirm({
                    message: `Are you sure you want to <b>delete</b> ${profileToDelete.firstname}'s profile? This will also delete all associated data.`,
                    type: "is-danger",
                    confirmText: 'Delete Profile',
                    onConfirm: () =>  this.deleteProfile(profileToDelete)
                })
            },
            deleteProfile(profileToDelete){
                Api.deleteProfile(profileToDelete.id, localStorage.getItem('authToken'))
                    .then(() => {
                        this.profiles = this.profiles.filter((profile) => {
                            return profile.id != profileToDelete.id
                        })
                        if (profileToDelete.id == store.getters.getUserId) { NavBar.methods.logout()}
                        this.successToast("Deleted profile")
                    })
                    .catch(() => this.warningToast("Profile could not be deleted"))
            },
            resetSearchFields() {
                this.email = "";
                this.name = "";
                this.chosenActivityTypes = []
            },
            searchUser() {
                this.startIndex = 0;
                const searchParameters = this.getSearchParameters();
                Api.getUserProfiles(localStorage.getItem('authToken'), searchParameters).then(response => {
                    this.startIndex += DEFAULT_RESULT_COUNT;
                    this.profiles = response.data.results
                })
            },
            getSearchParameters() {
                const searchParameters = {count: DEFAULT_RESULT_COUNT, startIndex: this.startIndex};
                if (this.name.length !== 0) {
                    searchParameters.fullname = this.name
                }
                if (this.email.length !== 0) {
                    searchParameters.email = this.email
                }
                if (this.chosenActivityTypes.length > 0) {
                    searchParameters.activityTypes = this.chosenActivityTypes.join(",")
                    searchParameters.method = this.activitySearchType
                }
                return searchParameters
            },

            loadMoreProfiles() {
                if (this.moreProfilesExist) {

                    const searchParameters = this.getSearchParameters()
                    Api.getUserProfiles(localStorage.getItem('authToken'), searchParameters).then(response => {
                        if (response.data.results.length == 0) {
                            this.moreProfilesExist = false;
                        } else {
                            this.startIndex += DEFAULT_RESULT_COUNT
                            const profiles = response.data.results
                            this.profiles = [...this.profiles, ...profiles]
                        }
                    })
                }
            },
        }
    }
</script>

<style scoped>
    #results {
        padding-top: 4rem;
    }

    #noMatches {
        padding-top: 4rem;
        color: red;
    }

    .isVerticalCenter {
        display: flex;
        align-items: center;
        padding-right: 1rem;
    }
    .container {
      margin-top: 0px;
    }
</style>
