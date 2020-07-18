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
    import List from "../List";
    import axios from "axios";
    import toastMixin from "../../mixins/toastMixin";

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
            }
        },
        methods: {
            deleteCountry(chosenCountry) {
                this.chosenCountries = this.chosenCountries.filter(country => country != chosenCountry)
            },
            addCountry() {
                console.log("adding country")
                if (this.newCountry === ""){
                    this.warningToast("No country selected")
                } else if (this.chosenCountries.includes(this.newCountry)) {
                    this.warningToast("Country already in list")
                } else {
                    this.chosenCountries = [...this.chosenCountries, this.newCountry]
                }
            },
            submitCountries(){
                this.$parent.updateCountries(this.chosenCountries)
                this.$buefy.toast.open({
                    duration: 2000,
                    message: "Countries saved",
                    type: 'is-success',
                    position: 'is-top'
                })
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
                    .catch(error => console.log(error));
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