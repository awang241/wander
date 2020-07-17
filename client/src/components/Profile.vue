<template>
    <div class="container containerColor">
        <!-- Header -->
        <section class="hero level">
            <div class=" hero-body level-item">
                <div class="container containerColor">Hello! I am
                    <h1 class="title is-1">
                        {{ profile.firstname }} {{ profile.middlename }} {{ profile.lastname }}
                    </h1>
                    <br>
                    <h2 class="subtitle is-5">
                        {{ profile.nickname }}
                    </h2>
                </div>

                <b-button v-if="viewingOwnProfile"
                          @click="editProfile"
                          type="is-info">
                    Edit Profile
                </b-button>

            </div>
        </section>
        <!-- Social Media Count -->
        <nav class="level">
            <div class="level-item has-text-centered">
                <div>
                    <p class="heading">Countries</p>
                    <p class="title">5</p>
                </div>
            </div>
            <div class="level-item has-text-centered">
                <div>
                    <p class="heading">Following</p>
                    <p class="title">123</p>
                </div>
            </div>
            <div class="level-item has-text-centered">
                <div>
                    <p class="heading">Followers</p>
                    <p class="title">150</p>
                </div>
            </div>
            <div class="level-item has-text-centered">
                <div>
                    <p class="heading">Likes</p>
                    <p class="title">789</p>
                </div>
            </div>
        </nav>
        <div class="section-heading">
            <div class="center container containerColor">
                <p>{{ profile.bio }}</p>
            </div>
        </div>
            <div class="container containerColor has-same-height is-gapless">
                <div class="column">
                    <!-- Profile -->
                    <div class="card">
                        <div class="card-content">
                            <h3 class="title is-4">Profile</h3>

                            <div class="content">
                                <table class="table-profile">
                                    <caption hidden>Table of some basic profile data</caption>
                                    <tr>
                                        <th colspan="1" scope="col"></th>
                                        <th colspan="2" scope="col"></th>
                                    </tr>
                                    <tr>
                                        <td>Gender:</td>
                                        <td>{{ profile.gender }}</td>
                                    </tr>
                                    <tr v-if="profile.location">
                                        <td>Location:</td>
                                        <td>{{fullLocation}}</td>
                                    </tr>
                                    <tr>
                                        <td>Birthday:</td>
                                        <td>{{ profile.date_of_birth }}</td>
                                    </tr>
                                    <tr>
                                        <td>Primary Email:</td>
                                        <td>{{ profile.primary_email }}</td>
                                    </tr>

                                    <tr v-for="email in profile.additional_email" :key="email">
                                        <td>Additional Email:</td>
                                        <td>{{email}}</td>
                                    </tr>
                                    <tr>
                                        <td>Fitness Level:</td>
                                        <td>{{ fitnessStatement }}</td>
                                    </tr>

                                </table>
                            </div>
                            <br>
                        </div>
                    </div>
                </div>
            </div>


        <section class="section">
            <h3 class="center activityTypesTitle title is-2">Countries</h3>
            <div class="container containerColor">
                <div class="box" v-if="checkLengthPassports == true">
                    <h3 v-for="country in profile.passports" :key="country" class="title is-4">{{country}}</h3>
                </div>
                <div v-else class="box">
                    <h1>No passport countries chosen</h1>
                </div>
            </div>
        </section>
        <section class="section">
            <h3 class="center activityTypesTitle title is-2">Activity Types</h3>
            <div class="container containerColor">
                <div class="box" v-if="checkActivitiesLength == true">
                    <h3 v-for="activityType in profile.activities" :key="activityType" class="title is-4">
                        {{activityType}}</h3>
                </div>
                <div v-else class="box">
                    <h1>No activities created</h1>
                </div>
            </div>
        </section>
    </div>
</template>



<script>
    import api from '../Api';
    import router from "../router";
    import store from "../store";

    export default {
        name: "Profile",
        props: ['id'],
        data() {
            return {
                profile: {},
                store: store,
               // id: this.$route.params.id
            }
        },
        // watch: {
        //     '$route.params.id': function (id) {
        //         this.id = id
        //         this.getProfile()
        //     }
        // },
        methods: {
            editProfile(){
                router.push({path: '/EditProfile/' + store.getters.getUserId});
            },
            getProfile() {
                let tempId;
                if (this.id) {
                    tempId = this.id;
                } else {
                    tempId = this.$route.params.id;
                }
                api.getProfile(tempId, localStorage.getItem("authToken"))
                    .then((response) => {
                        this.profile = response.data;
                    })
                    .catch((error) => {
                        router.push({path: '/'});
                        console.log(error)
                    })
            },
            checkActivitiesLength() {
                if(this.profile.activities.length > 0) {
                    return true
                } else {
                    return false
                }
            },
            checkLengthPassports() {
                if(this.profile.activities.length > 0) {
                    return true
                } else {
                    return false
                }
            }
        },
        computed: {
            viewingOwnProfile() {
                return this.profile.id == store.getters.getUserId
            },
            fitnessStatement: function () {
                switch (this.profile.fitness_statement) {
                    case 0 :
                        return "Beginner: I am not active at all";
                    case 1 :
                        return "Novice: I do a low level of exercise (walking)";
                    case 2 :
                        return "Intermediate: I work out 1-2 times per week";
                    case 3 :
                        return "Advanced: I work out 3-4 times per week";
                    case 4 :
                        return "Pro: I work out 5+ times per week";
                    default:
                        return "Beginner: I am not active at all";
                }
            },
            fullLocation: function () {
                let locationString = this.profile.location.city + ", "
                if (this.profile.location.state) {
                    locationString += this.profile.location.state + ", "
                }
                locationString += this.profile.location.country
                return locationString
            }
        },
        mounted() {
            console.log(this.id)
            this.getProfile()

        },
    }
</script>

<style scoped>
    .containerColor {
        background-color: #F7F8F9
    }

    .center {
        text-align: center;
    }
</style>