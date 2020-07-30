<template>
    <div class="card">
        <div class="columns">
            <div class="column">
                <h4><strong>{{name}}</strong></h4>
                <div v-if="this.profileIsAdmin" class="color-primary">Admin</div>

                <p>{{profile.gender}}</p>
                <p>{{profile.email}}</p>
            </div>
            <div v-if="profile.activities.length > 0" class="column">
                <strong>Activity Types</strong>
                <div v-for="activity in profile.activities" :key="activity">
                    <p> {{activity}}</p>
                </div>
            </div>
            <b-menu-item v-if="store.getters.getAuthenticationLevel <= 1" class="is-vertical-center">
                <template slot="label">
                    <b-dropdown aria-role="list" class="is-pulled-right" position="is-bottom-left">
                        <b-icon icon="ellipsis-v" slot="trigger"></b-icon>
                        <b-dropdown-item aria-role="listitem" @click="gotoProfile">View profile</b-dropdown-item>
                        <b-dropdown-item aria-role="listitem" @click="editProfile">Edit profile </b-dropdown-item>
                        <b-dropdown-item aria-role="listitem" @click="deleteProfile">Delete profile</b-dropdown-item>
                        <b-dropdown-item v-if="this.profileIsAdmin" aria-role="listitem" @click="changeAdminRights('user')">Remove admin rights</b-dropdown-item>
                        <b-dropdown-item v-else aria-role="listitem" @click="changeAdminRights('admin')">Make admin</b-dropdown-item>
                    </b-dropdown>
                </template>
            </b-menu-item>
            <b-button v-else type="is-text" @click="gotoProfile(profile.id)">View profile</b-button>
        </div>
    </div>
</template>

<script>
    import Profile from "./Profile.vue";
    import EditProfile from "./editprofile/EditProfile"
    import Api from "../Api";
    import toastMixin from "../mixins/toastMixin";
    import store from "../store";

    export default {
        name: "ProfileSummary",
        mixins: [toastMixin],
        data() {
            return {
                profileData: {},
                store: store
            }
        },
        mounted() {
            this.profileData = this.props.profile;
        },
        computed: {
            name() {
                return `${this.profile.firstname}  ${this.profile.lastname}`
            },
            profileIsAdmin(){
                return this.profile.authLevel < 2
            }
        },
        props: {
            profile: {
                type: Object,
                required: true
            }
        },
        methods: {
            gotoProfile() {
                this.$buefy.modal.open({
                    parent: this,
                    props: {id: this.profile.id},
                    component: Profile,
                    trapFocus: true,
                    scroll: "clip"
                })
            },
            editProfile(){
                this.$buefy.modal.open({
                    parent: this,
                    props: {id: this.profile.id},
                    component: EditProfile,
                    trapFocus: true,
                    scroll: "clip"
                })
            },
            deleteProfile() {
                this.$buefy.dialog.confirm({
                    message: `Are you sure you want to <b>delete</b> ${this.profile.firstname}'s profile? This will also delete all associated data.`,
                    type: "is-danger",
                    confirmText: 'Delete Profile',
                    onConfirm: () =>  this.$emit('deleteClicked', this.profile.id)
                })
            },
            changeAdminRights(permissionLevel){
                this.$buefy.dialog.confirm({
                    message: `Are you sure you want to change ${this.profile.firstname}'s role to ${permissionLevel}?`,
                    confirmText: 'Yes',
                    onConfirm: () =>  {
                        Api.editProfilePermissions(this.profile.id, permissionLevel, localStorage.getItem("authToken"))
                            .then(() => {
                                if(permissionLevel === "admin"){
                                    this.successToast(`${this.profile.firstname} is now an admin`)
                                    this.profile.authLevel = 1
                                } else {
                                    this.successToast(`${this.profile.firstname} is no longer an admin`)
                                    this.profile.authLevel = 5
                                }
                            })
                            .catch(() => this.warningToast(`Chould not change user to ${permissionLevel}`))
                    }
                })
            },
        }
    }
</script>

<style scoped>
    .columns {
        padding: 1rem;
    }
    li{
        list-style-type: none;
    }
    .is-vertical-center {
        display: flex;
        align-items: center;
        padding-right: 1rem;
    }

    .color-primary {
        color: #4099FF
    }
</style>