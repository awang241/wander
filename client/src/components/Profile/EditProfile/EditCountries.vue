<template>
    <div class="container">
        <h4 class="title is-5">Add Countries</h4>
        <b-field>
            <b-select placeholder="Select a country" v-model="newCountry" expanded>
                <option
                        v-for="country in possibleCountries"
                        :value="country"
                        :key="country">
                    {{ country }}
                </option>
            </b-select>
            <b-button type="is-primary" @click="addCountry">Add</b-button>
        </b-field>
        <List v-bind:chosenItems="chosenCountries" v-on:deleteListItem="deleteCountry"></List>
        <br>
        <b-button style="float:right" type="is-primary" @click="submitCountries">Save</b-button>
        <br>
    </div>
</template>

<script>
    import List from "../../Misc/HelperComponents/List";
    import axios from "axios";
    import toastMixin from "../../../mixins/toastMixin";

    export default {
        name: "EditCountries",
        mixins: [toastMixin],
        components: {List},
        props: ["profile"],
        data(){
            return {
                possibleCountries: "",
                newCountry: "",
                chosenCountries: this.profile.passports,
                originalCountries: this.profile.passports
            }
        },
        methods: {
            deleteCountry(chosenCountry) {
                this.chosenCountries = this.chosenCountries.filter(country => country != chosenCountry)
            },
            addCountry() {
                if (this.newCountry === ""){
                    this.warningToast("No country selected")
                } else if (this.chosenCountries.includes(this.newCountry)) {
                    this.warningToast("Country already in list")
                } else {
                    this.chosenCountries = [...this.chosenCountries, this.newCountry]
                }
            },
            submitCountries(){
                if (this.originalCountries !== this.chosenCountries) {
                    this.$parent.updateCountries(this.chosenCountries)
                    this.successToast("New countries saved")
                    this.originalCountries = this.chosenCountries
                } else {
                    this.warningToast("No changes made")
                }
            },
            getAllCountries(){
                axios.get("https://restcountries.eu/rest/v2/all")
                    .then(response => {
                        const data = response.data
                        const possibleCountries = []
                        for (let country in data){
                            possibleCountries.push(data[country].name)
                        }
                        this.possibleCountries = possibleCountries;
                    })
                    .catch(() => this.warningToast("Error occured while fetching REST API countries."));
            }
        },
        mounted() {
            this.getAllCountries()
        },
    }
</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
        margin-top: 0px;
        padding: 0px;
    }

</style>