<template>
    <div v-if="activity" class="container containerColor">
        <!-- Header -->
        <section>
            <div id="activity-key-info">
                <div>
                    <div v-if="hasShareAndEditPermissions()" class="buttons" style="float:right">
                        <b-button style="float:right;" @click="shareActivity"
                                  type="is-primary">
                            Share / Change Privacy Level
                        </b-button>
                        <b-button style="float:right" @click="editActivity"
                                  type="is-primary">
                            Edit Activity
                        </b-button>
                    </div>
                    <div v-else class="buttons" style="float:right">
                        <div class="buttons">
                        <b-button v-if="userRole !== 'follower'" style="float:right" @click="addRole('follower')"
                                 type="is-primary">
                            Follow
                        </b-button>
                        <b-button v-if="userRole === 'follower'" style="float:right" @click="deleteRole(store.getters.getUserId, 'follower')"
                                  type="is-danger">
                            Unfollow
                        </b-button>
                        <b-button v-if="userRole !== 'participant'" style="float:right" @click="addRole('participant')"
                                  type="is-primary">
                            Participate
                        </b-button>
                        <b-button v-if="userRole === 'participant'" style="float:right" @click="deleteRole(store.getters.getUserId, 'participant')"
                                  type="is-danger">
                            Unparticipate
                        </b-button>
                        </div>
                    </div>
                    <div v-if="userRole === 'organiser'" style="float:right; margin-right: 0.5rem;" class="buttons">
                        <b-button v-if="userRole === 'organiser'" style="float:right" @click="deleteRole(store.getters.getUserId,'organiser')"
                                  type="is-danger">
                            Remove self as organiser
                        </b-button>
                    </div>

                    <h1 class="title is-1">
                        {{activity.activity_name}}
                    </h1>
                    <h2 class="subtitle is-5">
                        Created by: {{ activity.creator }}
                    </h2>
                    <div>
                        <h3 class="title is-5">{{privacy}}</h3>
                    </div>
                </div>
            </div>
        </section>

        <div class="has-same-height is-gapless">
          <div>
                <div>
                    <div class="column">
                        <!-- Activities -->
                        <div class="card">
                            <div class="card-content">
                                <div class="content">
                                    <table class="table-profile">
                                        <caption hidden>Displayed Activity Table</caption>
                                        <tr>
                                            <th colspan="1" scope="col"></th>
                                            <th colspan="2" scope="col"></th>
                                        </tr>
                                        <tr>
                                            <td>Description:</td>
                                            <td>{{activity.description}}</td>
                                        </tr>
                                        <tr>
                                            <td>Continuous/Duration:</td>
                                            <td v-if="activity.continuous">continuous</td>
                                            <td v-else>duration</td>
                                        </tr>

                                        <tr v-if="!activity.continuous">
                                            <td>Start Time:</td>
                                            <td>UTC {{dateFormat(activity.start_time)}}</td>
                                        </tr>
                                        <tr v-if="!activity.continuous">
                                            <td>End Time:</td>
                                            <td>UTC {{dateFormat(activity.end_time)}}</td>
                                        </tr>

                                        <tr>
                                            <td>Location:</td>
                                            <td>{{activity.location}}</td>
                                        </tr>
                                        <tr v-for="type in activity.activity_type" :key="type">
                                            <td>Activity Type:</td>
                                            <td>{{type}}</td>
                                        </tr>
                                      <tr>
                                          <td>Followers:</td>
                                          <td>{{numFollowers}}</td>
                                      </tr>
                                    </table>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <div>
            <b-tabs v-model="roleIndex">
                <b-tab-item label="Organisers">
                    <div class="flex"
                         v-if="members[roles.ORGANISER].length > 0">
                        <div class="table-profile"
                             v-for="organiser in members.organiser"
                             :key="organiser.id">
                            <ProfileSummary class="flex-item" :profile="organiser">
                                <template #options>
                                    <b-dropdown aria-role="list" class="is-pulled-right" position="is-bottom-left" v-if="store.getters.getAuthenticationLevel <= 1">
                                        <b-icon icon="ellipsis-v" slot="trigger"></b-icon>
                                        <b-dropdown-item @click="changeRole(organiser, roles.ORGANISER, roles.PARTICIPANT)">Change to Participant</b-dropdown-item>
                                        <b-dropdown-item @click="deleteRole(organiser.id, roles.ORGANISER)">Remove from activity</b-dropdown-item>
                                    </b-dropdown>
                                </template>
                            </ProfileSummary>
                        </div>
                    </div>
                    <div v-else>
                        <p>This activity has no organisers.</p>
                    </div>
                </b-tab-item>

                <b-tab-item label="Participants">
                    <div class="flex">
                        <div v-if="members[roles.PARTICIPANT].length > 0">
                            <div class="table-profile"
                                 v-for="participant in members.participant"
                                 :key="participant.id">
                                <ProfileSummary class="flex-item" :profile="participant">
                                    <template #options>
                                        <b-dropdown aria-role="list" class="is-pulled-right" position="is-bottom-left">
                                            <b-icon icon="ellipsis-v" slot="trigger"></b-icon>
                                            <b-dropdown-item @click="changeRole(participant, roles.PARTICIPANT, roles.ORGANISER)">Change to Organiser</b-dropdown-item>
                                            <b-dropdown-item @click="deleteRole(participant.id, roles.PARTICIPANT)">Remove from activity</b-dropdown-item>
                                        </b-dropdown>
                                    </template>
                                </ProfileSummary>
                            </div>
                        </div>
                        <div v-else>
                            <p>This activity has no participants.</p>
                        </div>
                    </div>
                </b-tab-item>

                <b-tab-item label="Followers"
                            v-if="store.getters.getAuthenticationLevel <= 1">
                    <div class="flex"
                         v-if="members[roles.FOLLOWER].length > 0">
                        <div class="table-profile"
                             v-for="follower in members.follower"
                             :key="follower.id">
                            <ProfileSummary class="flex-item" :profile="follower">
                                <template #options>
                                    <b-dropdown aria-role="list" class="is-pulled-right" position="is-bottom-left">
                                        <b-icon icon="ellipsis-v" slot="trigger"></b-icon>
                                    </b-dropdown>
                                </template>
                            </ProfileSummary>
                        </div>
                    </div>
                    <div v-else>
                        <p>This activity has no followers.</p>
                    </div>
                </b-tab-item>
            </b-tabs>
        </div>
    </div>
</template>

<script>
    import ProfileSummary from "./ProfileSummary";
    import router from "../router";
    import api from "../Api";
    import store from '../store';
    import toastMixin from "../mixins/toastMixin";

    const ROLES = Object.freeze({
        CREATOR: "creator",
        ORGANISER: "organiser",
        PARTICIPANT: "participant",
        FOLLOWER: "follower"
    });

    export default {
        name: "ViewActivity",
        components: {ProfileSummary},
        mixins: [toastMixin],
        data() {
            return {
                roles: ROLES,

                userRole: null,

                activityId: this.$route.params.id,
                activity: null,
                members: {
                    "organiser": [],
                    "participant": [],
                    "follower": [],
                },
                organisers: [],
                participants: [],
                followers: [],
                roleIndex: 0,
                store: store,
                isCreatorOrOrganiser: false,
                numFollowers: 0
          }
        },
        methods: {
            getRoleCounts(){
                api.getRoleCountsForActivity(this.$route.params.id, localStorage.getItem('authToken'))
                .then(response => this.numFollowers = response.data.followers)
            },
            editActivity() {
                router.push({name: 'editActivity', params: {activityProp: this.activity}});
            },
            getActivity() {
                api.getActivity(this.activityId, localStorage.getItem('authToken'))
                    .then(response => this.activity = response.data)
                    .catch((error) => {
                        console.log(error);
                        router.go(-1);
                    })
            },
            getAllActivityMembers() {
                this.getActivityMembers(this.roles.PARTICIPANT);
                this.getActivityMembers(this.roles.ORGANISER);
                if (store.getters.getAuthenticationLevel <= 1) {
                    this.getActivityMembers(this.roles.FOLLOWER);
                }
            },
            getActivityMembers(role) {
                let searchParams = null;
                if (role === this.roles.PARTICIPANT && store.getters.getAuthenticationLevel > 1) {
                    searchParams = {
                        count: 50,
                        index: 0
                    };
                }
                api.getActivityMembers(this.activityId, role, localStorage.getItem('authToken'), searchParams)
                    .then(response => {this.members[role] = response.data.summaries;})
                    .catch((error) => {
                        console.log(error);
                        this.warningToast("Error loading activity data");
                    })
            },
            getRoleName: function (roleIndex) {
                switch (roleIndex) {
                    case 0:
                        return ROLES.ORGANISER;
                    case 1:
                        return ROLES.PARTICIPANT;
                    case 2:
                        return ROLES.FOLLOWER;
                    default:
                        return null
                }
            },
            hasShareAndEditPermissions() {
                return ((this.activity && this.activity.creatorId == this.store.getters.getUserId) || this.store.getters.getAuthenticationLevel < 2);
            },
            changeRole(profile, oldRole, newRole) {
                if (newRole !== oldRole) {
                    api.editActivityMemberRole(profile.id, this.activity.id, newRole, localStorage.getItem("authToken"))
                        .then(() => {
                            let index = this.members[oldRole].findIndex((item) => {return item === profile});
                            if (index !== -1) {
                                this.members[oldRole].splice(index, 1);
                                this.members[newRole].push(profile);
                            } else {
                                console.log("Error updating members locally. The program will now reload data from server");
                                this.getAllActivityMembers();
                            }
                            this.successToast("Role successfully updated.")
                        }).catch((error) => {
                        console.log(error)
                    })
                }
            },

            deleteRole(profileId, oldRole){
                  api.deleteActivityMembership(profileId, this.activity.id, localStorage.getItem("authToken"))
                    .then(() => {
                        this.members[oldRole] = this.members[oldRole].filter(member => member.id !== profileId)
                        this.successToast("Removed user from activity!")
                        if (profileId === this.store.getters.getUserId) {
                            this.userRole = null
                        }
                    })
                    .catch(() => this.warningToast("User could not be removed from the activity!"))
            },
            shareActivity() {
                router.push("/ShareActivity/" + this.activity.id + "/" + this.privacy.toLowerCase())
            },
            addRole(role) {
              api.addActivityRole(this.store.getters.getUserId, this.$route.params.id, localStorage.getItem('authToken'), role)
                  .then(() => {
                      this.userRole = role
                    this.successToast("Now a " + role)
                  })
                  .catch((error) => {
                    console.log(error)
                  })
            },
            dateFormat(date) {
                let year = date.slice(0, 4);
                let month = date.slice(5, 7);
                let day = date.slice(8, 10);
                let hour = date.slice(11, 13);
                let min = date.slice(14, 16);
                return hour + ":" + min + " " + day + "/" + month + "/" + year;
            },
            getUserRole() {
                api.getSingleUserActivityRole(this.store.getters.getUserId, this.$route.params.id, localStorage.getItem('authToken'))
                    .then(response => {
                        this.userRole = response.data.role})
            }
        },
        computed: {
            privacy: function () {
                switch (this.activity.privacyLevel) {
                    case 0 :
                        return "Private";
                    case 1 :
                        return "Friends";
                    case 2 :
                        return "Public";
                    default:
                        return "Private";
                }
            }
        },
        mounted() {
            this.getActivity();
            this.getAllActivityMembers();
            this.getRoleCounts();

            this.activity = {
                continuous: false
            };
            this.getUserRole()
        }
    }
</script>

<style scoped>
    .flex {
        display: flex;
        flex-direction: row;
        flex-wrap: wrap;
        justify-content: space-between;
    }
    .flex-item {
        margin: 20px 0;
        width: 500px;
    }

</style>