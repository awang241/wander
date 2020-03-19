<template>
    <div>
        <h4 class="title is-5">Add Countries</h4>
        <b-field>
            <b-select placeholder="Select a country" v-model="country">
                <option
                        v-for="country in possibleCountries"
                        :value="country"
                        :key="country">
                    {{ country }}
                </option>
            </b-select>
            <b-button type="is-primary" @click="addCountry">Submit</b-button>
        </b-field>
        <List v-bind:chosenItems="chosenCountries" v-on:deleteListItem="deleteCountry"></List>
    </div>
</template>

<script>
    import List from "../List";
    import axios from "axios";

    export default {
        name: "EditCountries",
        components: List,
        props: ["chosenCountries"],
        data(){
            return {
                possibleCountries: ""
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
            deleteCountry(chosenCountry) {
                this.chosenCountries = this.chosenCountries.filter(country => country != chosenCountry)
            },
            addCountry(newCountry) {
                if (!this.chosenCountries.includes(newCountry.name)) {
                    this.chosenCountries = [...this.chosenCountries, newCountry.name]
                } else {
                    this.showCountryInListWarning()
                }
            }
        },
        mounted() {
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