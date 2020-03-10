<template>
    <div class="container">
            <section class="hero is-primary">
                <div class="hero-body">
                    <div class="container">
                        <h1 class="title">
                           {{ firstName }} {{ middleName }} {{ lastName }}
                        </h1>
                        <h2 class="subtitle">
                            SamNZ
                        </h2>
                    </div>
                </div>
            </section>
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
        <div class="tile is-ancestor">
            <div class="tile is-vertical is-8">
                <div class="tile">
                    <div class="tile is-parent is-vertical">
                        <article class="tile is-child notification is-primary">
                            <p class="title">Personal</p>
                            <p class="subtitle"> Birthday: {{ dateOfBirth }} </p>
                            <p class="subtitle"> Gender: {{ gender }} </p>
                            <p class="subtitle"> Email: {{ email }} </p>
                            <p class="subtitle"> Fitness Level: To add </p>

                        </article>
                        <article class="tile is-child notification is-warning">
                            <p class="title">Bio</p>
                            <p class="subtitle"> {{ bio }}</p>
                        </article>
                    </div>
                    <div class="tile is-parent">
                        <article class="tile is-child notification is-info">
                            <p class="title"></p>
                            <p class="subtitle"></p>
                            <figure class="image is-4by3">
                                <img src="https://bulma.io/images/placeholders/640x480.png">
                            </figure>
                        </article>
                    </div>
                </div>
                <div class="tile is-parent">
                    <article class="tile is-child notification is-danger">
                        <p class="title">Activities</p>
                        <p class="subtitle">Biking, Rock climbing</p>
                        <div class="content">
                            <!-- Content -->
                        </div>
                    </article>
                </div>
            </div>
            <div class="tile is-parent">
                <article class="tile is-child notification is-success">
                    <div class="content">
                        <p class="title">Countries</p>
                        <p class="subtitle"></p>
                        <li v-for="country in chosenCountries" v-bind:key="country"> {{ country }}</li>
                        <div class="content">
                            <!-- Content -->
                        </div>
                    </div>
                </article>
            </div>
        </div>
            <div>
                <ul>
                    <li> {{ firstName }} {{ middleName }} {{ lastName }} </li>
                    <li> {{ nickName }}</li>
                    <li> Date of Birth: {{ dateOfBirth }} </li>
                    <li> Gender: {{ gender }} </li>
                    <li> Email: {{ email }} </li>
                    <li> Bio: {{ bio }} </li>
                    <li v-for="country in chosenCountries" v-bind:key="country"> {{ country }}</li>
                </ul>
            </div>
<!--            <li v-for="(value, key) in currentUser" v-bind:key="key">-->
<!--                {{key}} : {{value}}-->
<!--            </li>-->




        <AddCountry v-bind:possibleCountries="possibleCountries" v-bind:chosenCountries="chosenCountries" v-on:addCountry="addCountry"></AddCountry>

        <countries v-bind:chosenCountries="chosenCountries" v-on:deleteCountry="deleteCountry"></countries>

    </div>
</template>

<script>
    import axios from 'axios'
    import api from '../Api';
    import AddCountry from "./AddCountry";
    import Countries from "./Countries";

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
                email: "bobby@google.com",
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
            }
        },
        mounted() {
            // Retrieves user data using their id number. Will change to token at some point
            api.getProfile(270)
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
                    this.chosenCountries = response.data.passport_countries;
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
    /* .container-1{*/
    /*    background-color: #FFFFFF;*/
    /*    align-items: center;*/
    /*    padding: 0 40px;*/
    /*    text-align: left;*/
    /*}*/

</style>