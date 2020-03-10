<template>
    <div class="container">
        <ul>
            <li v-for="(value, key) in currentUser" v-bind:key="key">
                {{key}} : {{value}}
            </li>
        </ul>


        <AddCountry v-bind:possibleCountries="possibleCountries" v-bind:chosenCountries="chosenCountries" v-on:addCountry="addCountry"></AddCountry>

        <countries v-bind:chosenCountries="chosenCountries" v-on:deleteCountry="deleteCountry"></countries>

    </div>
</template>

<script>
    import axios from 'axios'
    import AddCountry from "./AddCountry";
    import Countries from "./Countries";

    export default {
        name: "Profile",
        components: {AddCountry, Countries},
        data() {
            return {
                currentUser: null,
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
            axios.get("https://f91246de-53d1-425e-9b1b-5524c2b62a0e.mock.pstmn.io/getusers")
                .then((response) => {
                    let rows =  response.data['users']
                    for(let i=0, len=rows.length; i<len; i++){
                        if(rows[i].email === this.email){
                            this.currentUser = rows[i]
                        }
                    }
                })
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