<template>
    <div class="container containerColor">
        <!-- Header -->
        <section class="hero level">
            <div class=" hero-body level-item">
                <div class="container containerColor">Hello! I am
                    <h1 class="title is-1">
                        {{ firstName }} {{ middleName }} {{ lastName }}
                    </h1>
                    <h2 class="subtitle is-3">
                        {{ nickName }}
                    </h2>

                </div>

                <b-button  @click="editProfile"
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
                <p>{{ bio }} Temporary bio templated, until user bio data can be inputted. I like rock climbing and merge conflicts. gg.</p>
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
                                        <td>{{ gender }}</td>
                                    </tr>
                                    <tr>
                                        <td>Birthday:</td>
                                        <td>{{ dateOfBirth }}</td>
                                    </tr>
                                    <tr>
                                        <td>Email:</td>
                                        <td>{{ primaryEmail }}</td>
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
                                                <strong>{{ fitness_statement }}</strong>
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
        <!-- Activities -->
        <section class="section" id="services">
            <div class="section-heading">
                <h3 class="center activitiesTitle title is-3">Activities</h3>
                <h4 class="subtitle is-5"></h4>
            </div>
            <div class="container containerColor">
                <div class="columns">
                    <div class="column">
                        <div class="box">
                            <div class="content">
                                <h4 class="title is-5">Rock Climbing</h4>
                            </div>
                        </div>
                    </div>
                    <div class="column">
                        <div class="box">
                            <div class="content">
                                <h4 class="title is-5">Tennis</h4>
                            </div>
                        </div>
                    </div>
                    <div class="column">
                        <div class="box">
                            <div class="content">
                                <h4 class="title is-5">Football</h4>
                            </div>
                        </div>
                    </div>

                    <div class="column">
                        <div class="box">
                            <div class="content">
                                <h4 class="title is-5">Basketball</h4>
                            </div>
                        </div>
                    </div>

                </div>

                <div class="columns">
                    <div class="column">
                        <div class="box">
                            <div class="content">
                                <h4 class="title is-5">Basketball</h4>
                            </div>
                        </div>
                    </div>
                    <div class="column">
                        <div class="box">
                            <div class="content">
                                <h4 class="title is-5">Hiking</h4>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </section>

        <section class="section">
            <div class="section-heading">
                <h3 class="center activitiesTitle title is-2">Countries</h3>
                <h4 class="subtitle is-5"></h4>
            </div>
            <div class="container containerColor">
                <div class="box">
                    <h3 v-for="country in chosenCountries" :key="country" class="title is-4">{{country}}</h3>
                </div>
            </div>
        </section>
    </div>
</template>

<script>
    import api from '../Api';
    import authenticationStore from "../store/authenticationStore";
    import profileStore from "../store/profileStore";
    import router from "../router";

    export default {
        name: "Profile",
        data() {
            return {
                currentUser: null,
                firstName: null,
                lastName: null,
                middleName: null,
                nickName: null,
                dateOfBirth: null,
                gender: null,
                bio: null,
                primaryEmail: null,
                additionalEmails: [],
                fitness_level: null,
                fitness_statement: null,
                possibleCountries: [],
                chosenCountries: []
            }
        },
        methods: {

            editProfile(){
                router.push('EditProfile');
            },
        },
        mounted() {
            // Retrieves user data using their id number. Will change to token at some point
            api.getProfile(authenticationStore.methods.getUserId(), authenticationStore.methods.getSessionId())
                .then((response) => {
                    //Save to auth store
                    profileStore.methods.setProfile(response.data)

                    this.firstName = response.data.firstname;
                    this.lastName = response.data.lastname;
                    this.middleName = response.data.middlename;
                    this.nickName = response.data.nickname;
                    this.dateOfBirth = response.data.date_of_birth;
                    this.gender = response.data.gender;
                    this.bio = response.data.bio;
                    this.primaryEmail = response.data.primary_email;
                    this.additionalEmails = response.data.additional_email;
                    this.fitness_level = response.data.fitness_level;
                    this.chosenCountries = response.data.passport_countries;
                    switch(response.data.fitness_level) {
                        case 0 :
                            this.fitness_statement = "Beginner: I am not active at all";
                            break;
                        case 1 :
                            this.fitness_statement = "Novice: I do a low level of exercise (walking)";
                            break;
                        case 2 :
                            this.fitness_statement = "Intermediate: I work out 1-2 times per week";
                            break;
                        case 3 :
                            this.fitness_statement = "Advanced: I work out 3-4 times per week";
                            break;
                        case 4 :
                            this.fitness_statement = "Pro: I work out 5+ times per week";
                            break;
                        default:
                            this.fitness_statement = "Beginner: I am not active at all";
                    }
                })
                .catch(error => console.log(error));
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