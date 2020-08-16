<template>
    <div v-if="activity" class="container containerColor">
        <!-- Header -->
        <section>
            <div id="activity-key-info">
                <div>
                    <div style="float:right">
                        <div v-if="hasShareAndEditPermissions" class="buttons">
                            <b-button style="float:right;" id="shareButton" @click="shareActivity"
                                      type="is-primary">
                                Share / Change Privacy Level
                            </b-button>
                            <b-button id="editButton" style="float:bottom" @click="editActivity"
                                      type="is-primary">
                                Edit Activity
                            </b-button>
                            <b-button style="float:right" @click="createParticipation"
                                      type="is-primary">
                                Create Participation
                            </b-button>
                        </div>
                        <div v-if="parseInt(activity.creatorId) !== parseInt(store.getters.getUserId) && userRole !== 'organiser'" class="buttons">
                            <div class="buttons">
                                <b-button v-if="userRole !== 'follower'" style="float:right" @click="updateRole(store.getters.getUserId,'follower')"
                                          id="followButton" type="is-primary">
                                    Follow
                                </b-button>
                                <b-button id="unfollowButton" v-if="userRole === 'follower'" style="float:right" @click="deleteRole(store.getters.getUserId, 'follower')"
                                          type="is-danger">
                                    Unfollow
                                </b-button>
                                <b-button id="participateButton" v-if="userRole !== 'participant'" style="float:right" @click="updateRole(store.getters.getUserId, 'participant')"
                                          type="is-primary">
                                    Participate
                                </b-button>
                                <b-button id="unparticipateButton" v-if="userRole === 'participant'" style="float:right" @click="deleteRole(store.getters.getUserId, 'participant')"
                                          type="is-danger">
                                    Unparticipate
                                </b-button>
                            </div>
                        </div>
                        <div v-if="userRole === 'organiser'" style="margin-right: 0.5rem;" class="buttons">
                            <b-button v-if="userRole === 'organiser'" style="float:right" @click="deleteRole(store.getters.getUserId,'organiser')"
                                      type="is-danger">
                                Remove self as organiser
                            </b-button>
                        </div>
                    </div>

                    <h1 class="title is-1">
                        {{activity.activity_name}}
                    </h1>
                    <h2 class="subtitle is-5">
                        Created by: {{ activity.creator }}
                    </h2>
                        <h2 v-if="userRole != null" class="subtitle is-5">
                            My Role: {{userRole}}
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
                                <h3 class="title is-4">{{activity.activity_name}}</h3>
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
                                            <td>{{dateFormat(activity.start_time)}}</td>
                                        </tr>
                                        <tr v-if="!activity.continuous">
                                            <td>End Time:</td>
                                            <td>{{dateFormat(activity.end_time)}}</td>
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
            <b-tabs v-model="tabIndex">
                <b-tab-item label="Organisers">
                    <div class="flex"
                         v-if="members[roles.ORGANISER].length > 0">
                        <div class="table-profile"
                             v-for="organiser in members.organiser"
                             :key="organiser.id">
                            <ProfileSummary class="flex-item" :profile="organiser">
                                <template v-if="hasShareAndEditPermissions" #options>
                                    <b-dropdown aria-role="list" class="is-pulled-right" position="is-bottom-left" v-if="store.getters.getAuthenticationLevel <= 1">
                                        <b-icon icon="ellipsis-v" slot="trigger"/>
                                        <b-dropdown-item @click="changeRole(organiser.id, roles.ORGANISER, roles.PARTICIPANT)">Change to Participant</b-dropdown-item>
                                        <b-dropdown-item @click="changeRole(organiser.id, roles.ORGANISER, roles.FOLLOWER)">Change to Follower</b-dropdown-item>
                                        <b-dropdown-item @click="deleteRole(organiser.id, roles.ORGANISER)">Remove from activity</b-dropdown-item>
                                    </b-dropdown>
                                </template>
                                <template v-else-if="parseInt(organiser.id) === parseInt(store.getters.getUserId)" #options>
                                    <b-button @click="deleteRole(organiser.id, roles.PARTICIPANT)"
                                              type="is-danger">
                                        Remove role
                                    </b-button>
                                </template>
                            </ProfileSummary>
                        </div>
                    </div>
                    <div v-else>
                        <p>This activity has no organisers.</p>
                    </div>
                </b-tab-item>

                <b-tab-item label="Participants">
                    <div class="flex"
                         v-if="members[roles.PARTICIPANT].length > 0 && members[roles.PARTICIPANT].length < 50">
                            <div class="table-profile"
                                 v-for="participant in members.participant"
                                 :key="participant.id">
                                <ProfileSummary class="flex-item" :profile="participant">
                                    <template v-if="hasShareAndEditPermissions" #options>
                                        <b-dropdown aria-role="list" class="is-pulled-right" position="is-bottom-left">
                                            <b-icon icon="ellipsis-v" slot="trigger"/>
                                            <b-dropdown-item @click="changeRole(participant.id, roles.PARTICIPANT, roles.ORGANISER)">Change to Organizer</b-dropdown-item>
                                            <b-dropdown-item @click="changeRole(participant.id, roles.PARTICIPANT, roles.FOLLOWER)">Change to Follower</b-dropdown-item>
                                            <b-dropdown-item @click="deleteRole(participant.id, roles.PARTICIPANT)">Remove from activity</b-dropdown-item>
                                        </b-dropdown>
                                    </template>
                                    <template v-else-if="parseInt(participant.id) === parseInt(store.getters.getUserId)" #options>
                                        <b-button @click="deleteRole(participant.id, roles.PARTICIPANT)"
                                                  type="is-danger">
                                            Remove role
                                        </b-button>
                                    </template>
                                </ProfileSummary>
                            </div>
                    </div>
                    <div v-else-if="members[roles.PARTICIPANT].length >= 50">
                        <p>Total number of participants: {{members[roles.PARTICIPANT].length}}</p>
                    </div>
                    <div v-else>
                        <p>This activity has no participants.</p>
                    </div>
                    <observer v-on:intersect="loadMoreProfiles(roles.PARTICIPANT)"/>
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
                                        <b-icon icon="ellipsis-v" slot="trigger"/>
                                        <b-dropdown-item @click="changeRole(follower.id, roles.FOLLOWER, roles.ORGANISER)">Change to Organizer</b-dropdown-item>
                                        <b-dropdown-item @click="changeRole(follower.id, roles.FOLLOWER, roles.PARTICIPANT)">Change to Participant</b-dropdown-item>
                                        <b-dropdown-item @click="deleteRole(follower.id, roles.FOLLOWER)">Remove from activity</b-dropdown-item>
                                    </b-dropdown>
                                </template>
                            </ProfileSummary>
                        </div>
                    </div>
                    <div v-else>
                        <p>This activity has no followers.</p>
                    </div>
                    <observer v-on:intersect="loadMoreProfiles(roles.FOLLOWER)"/>
                </b-tab-item>

                <b-tab-item label="Results">
                    <div class="flex" v-if="participationResults.length > 0">
                        <div class="table-profile"
                             v-for="participationResult in participationResults" :key="participationResult.id">
                            <ActivityParticipationSummary class="flex-item" :result="participationResult">
                            </ActivityParticipationSummary>
                        </div>
                    </div>
                    <div v-else>
                        <p>This activity has no participation results.</p>
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
    import Observer from "./Observer";
    import ActivityParticipationSummary from "./ActivityParticipationSummary";
    import toastMixin from "../mixins/toastMixin";


    const DEFAULT_RESULT_COUNT = 50;

    const ROLES = Object.freeze({
        CREATOR: "creator",
        ORGANISER: "organiser",
        PARTICIPANT: "participant",
        FOLLOWER: "follower"
    });

    function RolePagingData() {
        this.startIndex = DEFAULT_RESULT_COUNT;
        this.finished = false;
    }

    export default {
        name: "ViewActivity",
        components: {ProfileSummary, ActivityParticipationSummary, Observer},
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
                pagingData: {
                    "organiser": new RolePagingData(),
                    "participant": new RolePagingData(),
                    "follower": new RolePagingData(),
                },
                tabIndex: 0,
                store: store,
                isCreatorOrOrganizer: false,
                numFollowers: 0,
                numParticipants: 0,
                moreOrganisersExist: true,
                moreParticipantsExist: true,
                moreFollowersExist: true,
                participationResults: [],
                myRole: "None"
          }
        },
        methods: {
            updateRole(profileId, newRole) {
                if (this.userRole == null) {
                    this.addRole(newRole);
                } else if (this.userRole !== newRole) {
                    this.changeRole(profileId, this.userRole, newRole);
                }
            },
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
                    .catch(() => {
                        this.warningToast("Error occurred.");
                        router.go(-1);
                    })
            },
            getAllActivityMembers() {
                this.getActivityMembers(this.roles.PARTICIPANT);
                this.getActivityMembers(this.roles.ORGANISER);
                    if (this.store.getters.getAuthenticationLevel <= 1) {
                        this.getActivityMembers(this.roles.FOLLOWER);
                    }

            },
            getActivityMembers(role) {
                let searchParams = null;
                if (role === this.roles.PARTICIPANT && store.getters.getAuthenticationLevel > 1) {
                    searchParams = {
                        count: DEFAULT_RESULT_COUNT,
                        index: 0
                    };
                }
                api.getActivityMembers(this.activityId, role, localStorage.getItem('authToken'), searchParams)
                    .then(response => {
                        this.members[role] = response.data.summaries;
                    })
                    .catch(() => {
                        this.warningToast("Error loading activity data.");
                    })
            },
            changeRole(profileId, oldRole, newRole) {

                if (newRole !== oldRole) {
                    api.editActivityMemberRole(profileId, this.activity.id, newRole, localStorage.getItem("authToken"))
                        .then(() => {
                            let profile = this.members[oldRole].find((profile) => {return profile.id === profileId});
                            this.members[oldRole] = this.members[oldRole].filter((profile) => profile.id !== profileId)
                            if (profile != null) {
                                this.members[newRole].push(profile);
                            } else {
                                this.getAllActivityMembers();
                            }
                            if (profileId == this.store.getters.getUserId) {
                                this.userRole = newRole
                            }
                            this.getRoleCounts();
                            this.successToast("Role successfully updated.")
                        }).catch((error) => {
                        this.warningToast(error);
                    })
                }
            },

            deleteRole(profileId, oldRole){
                api.deleteActivityMembership(profileId, this.activity.id, localStorage.getItem("authToken"))
                    .then(() => {
                        this.members[oldRole] = this.members[oldRole].filter(member => member.id !== profileId)
                        this.successToast("Removed user from activity!")
                        if (parseInt(profileId) === parseInt(store.getters.getUserId)) {
                            this.userRole = null
                        }
                        this.getRoleCounts();
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
                        this.getActivityMembers(role)
                        this.successToast("Now a " + role)
                        this.getRoleCounts();
                    })
                    .catch((error) => {
                        this.warningToast(error);
                    })
            },
            createParticipation() {
                router.push('/Activities/' + this.activity.id + '/Participation')
            },
            dateFormat(date) {
                if (date) {
                    let year = date.slice(0, 4);
                    let month = date.slice(5, 7);
                    let day = date.slice(8, 10);
                    let hour = date.slice(11, 13);
                    let min = date.slice(14, 16);
                    return hour + ":" + min + " " + day + "/" + month + "/" + year;
                }

            },
            getUserRole() {
                api.getSingleUserActivityRole(localStorage.getItem('userId'), this.$route.params.id, localStorage.getItem('authToken'))
                    .then(response => {
                        this.userRole = response.data.role})
            },
            loadMoreProfiles(role) {
                const pagingData = this.pagingData[role];
                if (!pagingData.finished && this.store.getters.getAuthenticationLevel < 2) {
                    const searchParameters = this.getSearchParameters(pagingData.startIndex, role);
                    api.getActivityMembers(this.activityId, role, localStorage.getItem("authToken"), searchParameters)
                        .then(response => {
                            if (response.data.summaries.length < DEFAULT_RESULT_COUNT) {
                                pagingData.finished = true;
                            }
                            pagingData.startIndex += DEFAULT_RESULT_COUNT;
                            const profiles = response.data.summaries;
                            this.members[role] = [...this.members[role], ...profiles]
                        })
                }
            },
            getSearchParameters(index, role) {
                return {count: DEFAULT_RESULT_COUNT, index: index, role: role}
            },
            getParticipationResults() {
                api.getAllActivityParticipations(this.$route.params.id, localStorage.getItem('authToken'))
                    .then(response => {
                        this.participationResults = response.data.results
                        })
            },
            getMyRole(){
              api.getMyActivityRole(this.$route.params.id, localStorage.getItem('authToken'))
                  .then(response => {
                    this.myRole = response.data.results
                  })
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
            },
            hasShareAndEditPermissions: function () {
                return ((this.activity && parseInt(this.activity.creatorId) === parseInt(this.store.getters.getUserId)) || this.store.getters.getAuthenticationLevel < 2);
            }
        },
        mounted() {
            this.getActivity()
            this.getRoleCounts()
            this.getParticipationResults()
            this.getMyRole()
            this.getAllActivityMembers();

            setTimeout(() => {
                this.getUserRole()
            }, 400);

            this.activity = {
                continuous: false
            };

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
        width: 490px;
        align-items: center;
        padding-right: 1rem;
    }

</style>