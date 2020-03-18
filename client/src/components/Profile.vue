<template>
    <div class="container containerColor">
        <!-- Header -->
        <section class="hero">
            <div class="containerColor hero-body">
                <div class="container containerColor">Hello! I am
                    <h1 class="title is-1">
                        {{ firstName }} {{ middleName }} {{ lastName }}
                    </h1>
                    <h2 class="subtitle is-3">
                        {{ nickName }}
                    </h2>
                    <button @click="editProfile">Edit Profile</button>
                </div>
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
                                        <td>{{ email }}</td>
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
                                                <br>
                                                <b-progress type="is-primary" :value=this.fitness_level max="5"></b-progress>
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
                    <h3 class="title is-4">Add a country</h3>
                    <AddCountry v-bind:possibleCountries="possibleCountries" v-bind:chosenCountries="chosenCountries" v-on:addCountry="addCountry"></AddCountry>
                    <countries v-bind:chosenItems="chosenCountries" v-on:deleteListItem="deleteCountry"></countries>
                </div>
            </div>
        </section>
    </div>


    <!--            <li v-for="(value, key) in currentUser" v-bind:key="key">-->
    <!--                {{key}} : {{value}}-->
    <!--            </li>-->





</template>

<script>
    import axios from 'axios'
    import api from '../Api';
    import AddCountry from "./AddCountry";
    import Countries from "./List";
    import authenticationStore from "../store/authenticationStore";
    import router from "../router";

    export default {
        name: "Profile",
        components: {AddCountry, Countries},
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
                email: null,
                fitness_level: null,
                fitness_statement: null,
                possibleCountries: [],
                chosenCountries: []
            }
        },
        methods: {
            showCountryInListWarning() {
                this.$buefy.snackbar.open({
                    duration: 5000,
                    message: 'Country is already in list',
                    type: 'is-danger',
                    position: 'is-bottom-left',
                    queue: false,
                })
            },
            deleteCountry(chosenCountry){
                this.chosenCountries = this.chosenCountries.filter(country => country != chosenCountry)
            },
            addCountry(newCountry){
                if(!this.chosenCountries.includes(newCountry.name)){
                    this.chosenCountries = [...this.chosenCountries, newCountry.name]
                } else {
                    this.showCountryInListWarning()
                }
            },
            editProfile(){
                console.log("editProfile clicked");
                router.push('editProfile');
            }
        },
        mounted() {
            // Retrieves user data using their id number. Will change to token at some point
            api.getProfile(authenticationStore.methods.getUserId(), authenticationStore.methods.getSessionId())
                .then((response) => {
                    console.log(response.data);
                    console.log(response.data.firstname)
                    this.firstName = response.data.firstname;
                    this.lastName = response.data.lastname;
                    this.middleName = response.data.middlename;
                    this.nickName = response.data.nickname;
                    this.dateOfBirth = response.data.date_of_birth;
                    this.gender = response.data.gender;
                    this.bio = response.data.bio;
                    this.email = response.data.email;
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
            // axios.get("https://f91246de-53d1-425e-9b1b-5524c2b62a0e.mock.pstmn.io/getusers")
            //     .then((response) => {
            //         let rows =  response.data['users']
            //         for(let i=0, len=rows.length; i<len; i++){
            //             if(rows[i].email === this.email){
            //                 this.currentUser = rows[i]
            //             }
            //         }
            //     })
            axios.get("https://restcountries.eu/rest/v2/all")
                .then(response => {
                    const data = response.data
                    const possibleCountries = []
                    for (let country in data){
                        possibleCountries.push(data[country].name)
                    }
                    this.possibleCountries = possibleCountries;
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