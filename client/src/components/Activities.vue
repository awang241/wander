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
        <div id="results" class="column" v-if="activities.length">
            <div
                    v-for="activity in activities"
                    :key="activity.id">
                <ActivitySummary :activity="activity" @deleteClicked="deleteActivity">
                </ActivitySummary>
                <br>
            </div>
        </div>


        <div v-else id="noMatches">
            <h1>No activities loaded!</h1>
        </div>
        <div>
            <component v-bind:is="component" v-bind:activities="activities"/>
        </div>

        <observer v-on:intersect="loadMoreActivities"></observer>
    </div>
</template>

<script>
import api from '../Api';
import router from "../router";
import store from "../store"
import toastMixin from "../mixins/toastMixin";
import Observer from "./Observer";
import {eventBus} from "../main";
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
                startIndex: 0,
                moreActivitiesExist: true,
                observer: null,
                activities: null,
                component: "",
            }
        },
        created() {
            //Used to update the list of profiles when a profile on this list is changed
            //This is done by watching for the profileWasEdited event on the global event bus
            eventBus.$on('activityWasEdited', (editedActivity) => {
                for (let i = 0; i < this.activities.length; i++) {
                    if(this.activities[i].id === editedActivity.id){
                        this.activities[i].activityName = editedActivity.activityName
                        this.activities[i].location = editedActivity.location
                        this.activities[i].activityTypes = editedActivity.activityTypes
                        this.activities[i].continuous = editedActivity.continuous
                        this.activities[i].creatorName = editedActivity.creatorName
                    }
                }
            })
        },
        methods: {
            //once we can get different privacy levels
            //just set the prop before method call to change component?
            changeToDiscoverActivities() {
                this.component = activityList;
            },
            changeToMyActivities() {
                this.component = activityList;
            },
            changeToFollowingActivities() {
                this.component = activityList;
            },
            changeToParticipatingActivities() {
                this.component = activityList;
            },
            getActivities() {
                api.getUserActivitiesList(store.getters.getUserId, localStorage.getItem('authToken'))
                    .then((response) => {
                        this.activities = response.data;
                        this.activities.sort(function (a, b) {
                                return a.continuous - b.continuous;
                            }
                        );
                    })
                    .catch(error => console.log(error));
            },
            goToAddActivity() {
                router.push({path: '/AddActivity'});
            }, editActivity(activity) {
                router.push({name: 'editActivity', params: {activityProp: activity}})
            }, activityDetail(activity) {
                router.push({path: 'Activities/' + activity.id})
            },
            deleteActivity(id) {
                api.deleteActivity(store.getters.getUserId, localStorage.getItem('authToken'), id)
                    .then((response) => {
                        console.log(response);
                        this.warningToast("Activity deleted")
                        this.activities = this.activities.filter(activity => activity.id != id);
                    })
                    .catch(error => console.log(error));
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
            getSearchParameters() {
              return {count: DEFAULT_RESULT_COUNT, startIndex: this.startIndex};
            },
            loadMoreActivities() {
              if (this.moreActivitiesExist) {
                const searchParameters = this.getSearchParameters();
                api.getNextActivities(store.getters.getUserId, localStorage.getItem("authToken"), searchParameters).then(response => {
                  console.log(response.data)
                  if (response.data.results.length == 0) {
                    this.moreActivitiesExist = false;
                  } else {
                    this.startIndex += DEFAULT_RESULT_COUNT;
                    const activities = response.data.results;
                    this.activities = [...this.activities, ...activities];
                  }
                })
              }
            }
        },
        mounted() {
            this.checkAuthenticationStatus();
            this.getActivities();
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

</style>