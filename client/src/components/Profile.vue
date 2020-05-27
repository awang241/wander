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
        <section class="section" id="about">

            <hr class ="hrLine">

            <div class="container containerColor has-same-height is-gapless">
                <div class="column">
                    <!-- Profile -->
                    <div class="card">
                        <div class="card-content">
                            <h3 class="title is-4">Profile</h3>

                            <div class="content">
                                <table class="table-profile">
                                    <tr>
                                        <th colspan="1"></th>
                                        <th colspan="2"></th>
                                    </tr>
                                    <tr>
                                        <td>Gender:</td>
                                        <td>{{ profile.gender }}</td>
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
                                </table>
                            </div>
                            <br>
                        </div>
                    </div>
                </div>

                <div class="column">
                    <!-- Fitness Level-->
                    <div class="card">
                        <div class="card-content skills-content">
                            <h3 class="title is-4">Fitness Level</h3>
                            <div class="content">
                                <article class="media">
                                    <div class="media-content">
                                        <div class="content">
                                            <p>
                                                <strong>{{ fitnessStatement }}</strong>
                                            </p>
                                        </div>
                                    </div>
                                </article>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </section>


        <section class="section">
            <div class="section-heading">
                <h3 class="center activityTypesTitle title is-2">Countries</h3>
                <h4 class="subtitle is-5"></h4>
            </div>
            <div class="container containerColor">
                <div class="box">
                    <h3 v-for="country in profile.passports" :key="country" class="title is-4">{{country}}</h3>
                </div>
            </div>
        </section>
        <section class="section">
            <div class="section-heading">
                <h3 class="center activityTypesTitle title is-2">Activity Types</h3>
                <h4 class="subtitle is-5"></h4>
            </div>
            <div class="container containerColor">
                <div class="box">
                    <h3 v-for="activityType in profile.activities" :key="activityType" class="title is-4">{{activityType}}</h3>
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
        data() {
            return {
                profile: {},
                store: store,
                id: this.$route.params.id
            }
        },
        watch: {
            '$route.params.id': function (id) {
                this.id = id
                this.getProfile()
            }
        },
        methods: {
            editProfile(){
                router.push({path: '/EditProfile/' + store.getters.getUserId});
            },
            getProfile() {
                api.getProfile(this.id, localStorage.getItem("authToken"))
                    .then((response) => {
                        this.profile = response.data;
                    })
                    .catch((error) => {
                        router.push({path: '/'});
                        console.log(error)
                    })
            }
        },
        computed: {
            // a computed getter
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
            }
        },
        mounted() {
            this.getProfile()

        },
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
        border:2px solid #EDEEEE;
    }

</style>