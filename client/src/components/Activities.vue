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
            <component v-bind:is="component" v-bind:activities="activities"/>
        </div>
    </div>
</template>

<script>
    import activityList from "./ActivityList";
    import Api from '../Api';
    import router from "../router";
    import store from "../store"
    import toastMixin from "../mixins/toastMixin";

    export default {
        name: "Activities",
        mixins: [toastMixin],
        data() {
            return {
                activities: null,
                store: store,
                component: "",
            }
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
                Api.getUserActivitiesList(store.getters.getUserId, localStorage.getItem('authToken'))
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
                Api.deleteActivity(store.getters.getUserId, localStorage.getItem('authToken'), id)
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
            }
        },
        mounted() {
            this.checkAuthenticationStatus();
            this.getActivities();
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