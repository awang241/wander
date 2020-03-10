<template>
    <div class="container">
        <ul>
            <header> Profile </header>
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
        </ul>


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
            deleteCountry(chosenCountry){
                this.chosenCountries = this.chosenCountries.filter(country => country != chosenCountry)
            },
            addCountry(newCountry){
                this.chosenCountries = [...this.chosenCountries, newCountry.name]
            }
        },
        mounted() {
            // Retrieves user data using their id number. Will change to token at some point
            api.getProfile(265)
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

</style>