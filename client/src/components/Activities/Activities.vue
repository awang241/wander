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
        <div class="has-same-height is-gapless">
            <b-tabs v-model="tabIndex" expanded>

                <b-tab-item label="My Activities">
                    <ActivityList v-on:loadMoreActivities="loadMoreActivities('creatorOrOrganiser')"
                                  v-bind:activities="myActivities" v-bind:role="'creatorOrOrganiser'"/>
                </b-tab-item>

                <b-tab-item label="Participating">
                    <ActivityList v-on:loadMoreActivities="loadMoreActivities('participant')"
                                  v-bind:activities="participatingActivities" v-bind:role="'participant'"/>
                </b-tab-item>

                <b-tab-item label="Following">
                    <ActivityList v-on:loadMoreActivities="loadMoreActivities('follower')"
                                  v-bind:activities="followingActivities" v-bind:role="'follower'"/>
                </b-tab-item>

                <b-tab-item label="Discover Activities">
                    <ActivityList v-on:loadMoreActivities="loadMoreActivities('discover')"
                                  v-bind:activities="discoverActivities" v-bind:role="'discover'"/>
                </b-tab-item>

            </b-tabs>
        </div>
    </div>
</template>

<script>
    import api from '../../Api';
    import router from "../../router";
    import store from "../../store"
    import toastMixin from "../../mixins/toastMixin";
    import ActivityList from "./ActivityHelpers/ActivityList";

    const DEFAULT_RESULT_COUNT = 5;


    export default {
        name: "Activities",
        mixins: [toastMixin],
        components: {ActivityList},
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
                myActivities: [],
                participatingActivities: [],
                followingActivities: [],
                discoverActivities: [],
                activities: this.myActivities,
                role: "creatorOrOrganiser",
                tabIndex: 0
            }
        },
        methods: {
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
                switch (role) {
                    case "creatorOrOrganiser":
                        if (this.moreMyActivitiesExist) {
                            let searchParameters = { count: DEFAULT_RESULT_COUNT, startIndex: this.myActivitiesStartIndex, role: role };
                            api.getNextActivities(this.store.getters.getUserId, localStorage.getItem("authToken"), searchParameters).then(response => {
                                if (response.data.results.length == 0) {
                                    this.moreMyActivitiesExist = false;
                                } else {
                                    this.myActivitiesStartIndex += DEFAULT_RESULT_COUNT;
                                    const activities = response.data.results;
                                    this.myActivities = [...this.myActivities, ...activities];
                                    this.myActivities.sort(function (a, b) {
                                            return a.continuous - b.continuous;
                                        }
                                    );
                                }
                            });
                        }
                        break;
                    case "participant":
                        if (this.moreParticipatingActivitiesExist) {
                            let searchParameters = { count: DEFAULT_RESULT_COUNT, startIndex: this.participatingActivitiesStartIndex, role: role };
                            api.getNextActivities(this.store.getters.getUserId, localStorage.getItem("authToken"), searchParameters).then(response => {
                                if (response.data.results.length == 0) {
                                    this.moreParticipatingActivitiesExist = false;
                                } else {
                                    this.participatingActivitiesStartIndex += DEFAULT_RESULT_COUNT;
                                    const activities = response.data.results;
                                    this.participatingActivities = [...this.participatingActivities, ...activities];
                                    this.participatingActivities.sort(function (a, b) {
                                            return a.continuous - b.continuous;
                                        }
                                    );
                                }
                            });
                        }
                        break;
                    case "follower":
                        if (this.moreFollowingActivitiesExist) {
                            let searchParameters = { count: DEFAULT_RESULT_COUNT, startIndex: this.followingActivitiesStartIndex, role: role };
                            api.getNextActivities(this.store.getters.getUserId, localStorage.getItem("authToken"), searchParameters).then(response => {
                                if (response.data.results.length == 0) {
                                    this.moreFollowingActivitiesExist = false;
                                } else {
                                    this.followingActivitiesStartIndex += DEFAULT_RESULT_COUNT;
                                    const activities = response.data.results;
                                    this.followingActivities = [...this.followingActivities, ...activities];
                                    this.followingActivities.sort(function (a, b) {
                                            return a.continuous - b.continuous;
                                        }
                                    );
                                }
                            });
                        }
                        break;
                    case "discover":
                        if (this.moreDiscoverActivitiesExist) {
                            let searchParameters = { count: DEFAULT_RESULT_COUNT, startIndex: this.discoverActivitiesStartIndex, role: role };
                            api.getNextActivities(this.store.getters.getUserId, localStorage.getItem("authToken"), searchParameters).then(response => {
                                if (response.data.results.length == 0) {
                                    this.moreDiscoverActivitiesExist = false;
                                } else {
                                    this.discoverActivitiesStartIndex += DEFAULT_RESULT_COUNT;
                                    const activities = response.data.results;
                                    this.discoverActivities = [...this.discoverActivities, ...activities];
                                    this.discoverActivities.sort(function (a, b) {
                                            return a.continuous - b.continuous;
                                        }
                                    );
                                }
                            });
                        }
                        break;
                }
            }
        },
        mounted() {
            this.checkAuthenticationStatus();

        },
        beforeMount() {
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

    #activities-key-info {
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