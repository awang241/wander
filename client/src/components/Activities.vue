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
            <div v-if="activities.length">
                <div v-for="activity in activities" v-bind:key="activity">
                    <div class="column">
                        <!-- Activities -->
                        <div class="card">
                            <div class="card-content">
                                <h3 class="title is-4">{{activity.activityName}}</h3>
                                Role: CREATOR
                                <b-button style="float: right" @click="activityDetail(activity)"
                                          type="is-text" >
                                    View more
                                </b-button>
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
                                            <td>Continous/Duration:</td>
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
                                    </table>
                                    <b-button style="float: left" @click="deleteActivity(activity.id)"
                                              type="is-danger">
                                        Delete
                                    </b-button>
                                    <b-button style="float: right" @click="editActivity(activity)"
                                              type="is-primary">
                                        Edit
                                    </b-button>
                                </div>
                                <br>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div v-else class="box">
                <h1>No activities created</h1>
            </div>
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

const DEFAULT_RESULT_COUNT = 3;

    export default {
        name: "Activities",
        mixins: [toastMixin],
      components: {Observer},
        data() {
            return {
                activities: [],
                store: store,
                startIndex: 0,
                moreActivitiesExist: true,
                observer: null
            }
        },
        methods: {
            goToAddActivity() {
                router.push({path: '/AddActivity'});
            }, editActivity(activity) {
                router.push({name: 'editActivity', params: {activityProp: activity}})
            }, activityDetail(activity) {
                router.push({path: 'Activities/' + activity.id})
            },
            deleteActivity(id) {
                console.log(id);
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
        }
    }

</script>

<style scoped>

    .containerColor {
        background-color: #F7F8F9
    }

    #activities-key-info{
        display: flex;
        justify-content: space-between;
        padding: 0 1rem;
    }

</style>