<template>
    <div v-if="store.getters.getAuthenticationStatus" class="container containerColor">
        <!-- Header -->
        <section>
            <br>
            <div id="activities-key-info">
                <div>
                    <h1 class="title is-1">
                        Activities
                    </h1>
                </div>
                <!-- redirect to add activity -->
                <div>
                    <b-button v-if="store.getters.getAuthenticationLevel > 0" @click="goToAddActivity"
                              type="is-primary">
                        Add Activity
                    </b-button>
                </div>
            </div>
        </section>
        <br>
        <br>
        <div class="has-same-height is-gapless">
            <div class="tabs is-centered">

                <ul>
                    <li><a v-on:click="changeToMyActivities">My Activities</a></li>
                    <li><a v-on:click="changeToParticipatingActivities">Participating</a></li>
                    <li><a v-on:click="changeToFollowingActivities">Following</a></li>
                    <li><a v-on:click="changeToDiscoverActivities">Discover Activities</a></li>

                </ul>
            </div>
        </div>


        <div>
            <component v-bind:is="component" v-bind:activities="activities" v-bind:role="role"/>
        </div>


    </div>
</template>

<script>
import api from '../Api';
import router from "../router";
import store from "../store"
import toastMixin from "../mixins/toastMixin";
import Observer from "./Observer";
import ActivitySummary from "./ActivitySummary";
import activityList from "./ActivityList";

const DEFAULT_RESULT_COUNT = 10;


    export default {
        name: "Activities",
        mixins: [toastMixin],
      components: {Observer, ActivitySummary},
        data() {
            return {
                store: store,
                discoverActivitiesStartIndex: 0,
                myActivitiesStartIndex: 0,
                participatingActivitiesStartIndex: 0,
                followingActivitiesStartIndex: 0,
                moreDiscoverActivitiesExist: true,
                moreMyActivitiesExist: true,
                moreFollowingActivitiesExist: true,
                moreParticipatingActivitiesExist: true,
                observer: null,
                activities: null,
                myActivities: [],
                participatingActivities: [],
                followingActivities: [],
                discoverActivities: [],
                component: "",
                role: ""
            }
        },
        methods: {
            //once we can get different privacy levels
            //just set the prop before method call to change component?
            changeToDiscoverActivities() {
                this.role = "discover";
                this.activities = this.discoverActivities;
                this.component = activityList;
            },
            changeToMyActivities() {
                this.role = "creatorOrOrganiser";
                this.activities = this.myActivities;
                this.component = activityList;
            },
            changeToFollowingActivities() {
                this.role = "follower";
                this.activities = this.followingActivities;
                this.component = activityList;
            },
            changeToParticipatingActivities() {
                this.role = "participant";
                this.activities = this.participatingActivities;
                this.component = activityList;
            },
            getActivities() {
                api.getNextActivities(store.getters.getUserId, localStorage.getItem('authToken'), this.getParameters())
                    .then((response) => {
                        this.activities = response.data;
                        this.activities.sort(function (a, b) {
                                return a.continuous - b.continuous;
                            }
                        );
                    })
                    .catch(() => this.warningToast("Error while fetching next activities."));
            },
            removeActivityFromList(activityId) {
                switch (this.role) {
                    case "follower":
                        this.followingActivities = this.followingActivities.filter((activity) => {
                            return activity.id !== activityId;
                        })
                        this.activities = this.followingActivities;
                        break;
                    case "participant":
                        this.participatingActivities = this.participatingActivities.filter((activity) => {
                            return activity.id !== activityId;
                        })
                        this.activities = this.participatingActivities;
                        break;
                    case "discover":
                        this.discoverActivities = this.discoverActivities.filter((activity) => {
                            return activity.id !== activityId;
                        })
                        this.activities = this.discoverActivities;
                        break;
                    case "creatorOrOrganiser":
                        this.myActivities = this.myActivities.filter((activity) => {
                            return activity.id !== activityId;
                        })
                        this.activities = this.myActivities;
                        break;
                }
            },
            goToAddActivity() {
                router.push({path: '/AddActivity'});
            },
            dateFormat(date) {
                let year = date.slice(0, 4);
                let month = date.slice(5, 7);
                let day = date.slice(8, 10);
                let hour = date.slice(11, 13);
                let min = date.slice(14, 16);
                return hour + ":" + min + " " + day + "/" + month + "/" + year;
            },
            checkAuthenticationStatus() {
                if (!store.getters.getAuthenticationStatus) {
                    router.push({path: '/'})
                }
            },
            getParameters(startIndex, role) {
              return {count: DEFAULT_RESULT_COUNT, startIndex: startIndex, role: role};
            },
            loadMoreActivities(role) {
                switch(role) {
                    case "creatorOrOrganiser":
                        if (this.moreMyActivitiesExist) {
                            const searchParameters = this.getParameters(this.myActivitiesStartIndex, role);
                            api.getNextActivities(store.getters.getUserId, localStorage.getItem("authToken"), searchParameters).then(response => {
                                if (response.data.results.length == 0) {
                                    this.moreMyActivitiesExist = false;
                                } else {
                                    this.myActivitiesStartIndex += DEFAULT_RESULT_COUNT;
                                    const activities = response.data.results;
                                    this.myActivities = [...this.myActivities, ...activities];
                                }
                            });
                        }
                        break;
                    case "participant":
                        if (this.moreParticipatingActivitiesExist) {
                            const searchParameters = this.getParameters(this.participatingActivitiesStartIndex, role);
                            api.getNextActivities(store.getters.getUserId, localStorage.getItem("authToken"), searchParameters).then(response => {
                                if (response.data.results.length == 0) {
                                    this.moreParticipatingActivitiesExist = false;
                                } else {
                                    this.participatingActivitiesStartIndex += DEFAULT_RESULT_COUNT;
                                    const activities = response.data.results;
                                    this.participatingActivities = [...this.participatingActivities, ...activities];
                                }
                            });
                        }
                        break;
                    case "follower":
                        if (this.moreFollowingActivitiesExist) {
                            const searchParameters = this.getParameters(this.followingActivitiesStartIndex, role);
                            api.getNextActivities(store.getters.getUserId, localStorage.getItem("authToken"), searchParameters).then(response => {
                                if (response.data.results.length == 0) {
                                    this.moreFollowingActivitiesExist = false;
                                } else {
                                    this.followingActivitiesStartIndex += DEFAULT_RESULT_COUNT;
                                    const activities = response.data.results;
                                    this.followingActivities = [...this.followingActivities, ...activities];
                                }
                            });
                        }
                        break;
                    case "discover":
                        if (this.moreDiscoverActivitiesExist) {
                            const searchParameters = this.getParameters(this.discoverActivitiesStartIndex, role);
                            api.getNextActivities(store.getters.getUserId, localStorage.getItem("authToken"), searchParameters).then(response => {
                                if (response.data.results.length == 0) {
                                    this.moreDiscoverActivitiesExist = false;
                                } else {
                                    this.discoverActivitiesStartIndex += DEFAULT_RESULT_COUNT;
                                    const activities = response.data.results;
                                    this.discoverActivities = [...this.discoverActivities, ...activities];
                                }
                            });
                        }
                        break;
                }
            }
        },
        mounted() {
            this.checkAuthenticationStatus();
            this.loadMoreActivities("creatorOrOrganiser");
            this.loadMoreActivities("participant");
            this.loadMoreActivities("follower");
            this.loadMoreActivities("discover");
        }
    }

</script>

<style scoped>
    .bannerColor {
        background-color: #64C6E3
    }

    .containerColor {
        background-color: #F7F8F9
    }

    .center {
        text-align: center;
    }

    .hrLine {
        border: 2px solid #EDEEEE;
    }

    #editButton {
        margin-left: 1rem;
    }

    #activities-key-info{
        display: flex;
        justify-content: space-between;
        padding: 0 1rem;
    }

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