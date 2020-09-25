<template>
    <div class="container containerColor">
        <!-- Header -->
        <section>
            <div id="profile-key-info">
                <div>Hello! I am
                    <h1 class="title is-1">
                        {{ profile.firstname }} {{ profile.middlename }} {{ profile.lastname }}
                    </h1>
                    <h2 v-if="profile.nickname" class="subtitle is-5">
                        ({{ profile.nickname }})
                    </h2>
                    <div id="profile-bio" v-if="profile.bio">
                        <h3 class="title is-5">{{ profile.bio }}</h3>
                    </div>

                </div>

                <div>
                    <b-button v-if="viewingOwnProfile"
                              @click="editProfile"
                              type="is-primary"
                              icon-left="user-edit">
                        Edit Profile
                    </b-button>
                </div>

            </div>
        </section>

        <div class="has-same-height is-gapless">
            <div class="column">
                <!-- Profile -->
                <div class="card">
                    <div class="card-content">
                        <h3 class="title is-4">
                            <b-icon icon="universal-access" style="color: #38C7FF;" custom-size="3x"></b-icon>
                            Basic information</h3>

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
                                    <td>{{profile.location.address}}</td>
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

                            <div v-if="profile.passports != null && profile.passports.length">
                                <br>
                                <h3 class="title is-4">
                                    <i class="fas fa-globe-asia iconPadding" style="font-size: 1.3em; color: #00d927"></i>
                                    Countries</h3>
                                <table class="table-profile">
                                    <caption hidden>Table of Passport countries for profile</caption>
                                    <tr>
                                      <th colspan="1" scope="col"></th>
                                    </tr>
                                    <tr v-for="country in profile.passports" :key="country">
                                        <td>{{country}}</td>
                                    </tr>
                                </table>
                            </div>
                            <br>
                            <div v-if="profile.activities != null && profile.activities.length">
                                <br>
                                <h3 class="title is-4">
                                    <i class="fas fa-heartbeat iconPadding" style="font-size: 1.3em; color:  #ff3838"></i>
                                    Activity Types</h3>
                                <table class="table-profile">
                                    <caption hidden>Table of activity types for profile</caption>
                                    <tr>
                                      <th colspan="1" scope="col"></th>
                                    </tr>
                                    <tr v-for="activityType in profile.activities" :key="activityType">
                                        <td>{{activityType}}</td>
                                    </tr>
                                </table>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>


<script>
    import api from '../../Api';
    import router from "../../router";
    import store from "../../store";

    export default {
        name: "Profile",
        props: ['id'],
        data() {
            return {
                profile: {},
                store: store,
            }
        },
        //Used to ensure the component updates to the new profile when the id is changed
        watch: {
            '$route.params.id': function (id) {
                this.id = id;
                this.getProfile()
            }
        },
        methods: {
            editProfile() {
                router.push({path: '/EditProfile/' + store.getters.getUserId});
            },
            getProfile() {
                if (store.getters.getAuthenticationLevel === 0) { router.push({path: '/'}) }
                api.getProfile(this.id, localStorage.getItem("authToken"))
                    .then((response) => {
                        this.profile = response.data;
                    })
                    .catch(() => {
                        if(store.getters.getAuthenticationStatus){
                            router.push({path: '/Profile/' + store.getters.getUserId})
                        } else {
                            router.push({path: '/Login'});
                        }
                    })
            }
        },
        computed: {
            viewingOwnProfile() {
                return this.profile.id === store.getters.getUserId;
            },
            fitnessStatement: function () {
                switch (this.profile.fitness) {
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
            }
        },
        mounted() {
            this.getProfile()

        },
    }
</script>

<style scoped>
    .containerColor {
        background-color: #F7F8F9
    }

    #profile-bio {
        margin: 0 0 1rem 0;
    }

    #profile-key-info{
        display: flex;
        justify-content: space-between;
        padding: 0rem 1rem;
    }

</style>