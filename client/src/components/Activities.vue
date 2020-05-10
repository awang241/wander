<template>
    <div v-if="store.getters.getAuthenticationStatus" class="container containerColor">
        <!-- Header -->
        <section class="hero level">
            <div class=" hero-body level-item">
                <div class="container containerColor">
                    <h1 class="title is-1">
                        Activities
                    </h1>
                </div>

                <!-- redirect to add activity -->
                <b-button @click="goToAddActivity"
                          type="is-info">
                    Add Activity
                </b-button>
            </div>
        </section>

        <hr class="hrLine">

        <div v-for="activity in activities" v-bind:key="activity">
            <div class="container containerColor has-same-height is-gapless">
                <div class="column">
                    <!-- Activities -->
                    <div class="card">
                        <div class="card-content">
                            <h3 class="title is-4">{{activity.activity_name}}</h3>
                            Role: CREATOR
                            <div class="content">
                                <table class="table-profile">
                                    <tr>
                                        <th colspan="1"></th>
                                        <th colspan="2"></th>
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
                                <b-button @click="deleteActivity(activity.id)"
                                          type="is-danger">
                                    Delete
                                </b-button>
                                <b-button class='px-3' id="editButton" @click="editActivity(activity)"
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
    </div>
</template>

<script>
    import api from '../Api';
    import router from "../router";
    import store from "../store"

    export default {
        name: "Activities",
        data() {
            return {
                activities: null,
                store: store
            }
        },
        methods: {
            getActivities() {
                api.getUserActivitiesList(store.getters.getUserId, localStorage.getItem('authToken'))
                    .then((response) => {
                        this.activities = response.data;
                        this.activities.sort(function(a,b){
                                return a.continuous - b.continuous;
                            }
                        );
                    })
                    .catch(error => console.log(error));
            },
            goToAddActivity() {
                router.push({path: '/AddActivity'});
            }, editActivity(activity){
                router.push({name: 'editActivity', params: {activityProp: activity}})
            },
            deleteActivity(id) {
                console.log(id);
                api.deleteActivity(store.getters.getUserId, localStorage.getItem('authToken'), id)
                    .then((response) => {
                        console.log(response);
                        this.$buefy.toast.open({
                            duration: 5500,
                            message: "Activity deleted",
                            type: 'is-danger',
                            position: 'is-top'
                        })
                        this.activities = this.activities.filter(activity => activity.id != id);
                    })
                    .catch(error => console.log(error));
            },
            dateFormat(date) {
                let year = date.slice(0, 4);
                let month = date.slice(5, 7);
                let day = date.slice(8, 10);
                let hour = date.slice(11, 13);
                let min = date.slice(17, 19);
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

    #editButton{
        margin-left: 1rem;
    }

</style>