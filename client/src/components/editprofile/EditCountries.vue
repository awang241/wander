<template>
    <div>
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
        <b-button type="is-primary" @click="submitCountries">Save</b-button>
    </div>
</template>

<script>
    import List from "../List";
    import axios from "axios";
    import Api from "../../Api";
    import profileStore from "../../store/profileStore";
    import authenticationStore from "../../store/authenticationStore";

    export default {
        name: "EditCountries",
        components: {List},
        data(){
            return {
                possibleCountries: "",
                newCountry: "",
                chosenCountries: profileStore.data.passportCountries,
            }
        },
        methods: {
            showWarning(message) {
                this.$buefy.snackbar.open({
                    duration: 5000,
                    message: message,
                    type: 'is-danger',
                    position: 'is-bottom-left',
                    queue: false,
                })
            },
            deleteCountry(chosenCountry) {
                this.chosenCountries = this.chosenCountries.filter(country => country != chosenCountry)
            },
            addCountry() {
                if (this.newCountry === ""){
                    this.showWarning("No country selected")
                } else if (this.chosenCountries.includes(this.newCountry)) {
                    this.showWarning("Country already in list")
                } else {
                    this.chosenCountries = [...this.chosenCountries, this.newCountry]
                }
            },
            submitCountries(){
                profileStore.methods.setPassportCountries(this.chosenCountries)
                const updatedProfile = {
                    "lastname": profileStore.data.lastName,
                    "firstname": profileStore.data.firstName,
                    "middlename": profileStore.data.middleName,
                    "nickname": profileStore.data.nickname,
                    "primary_email": profileStore.data.primaryEmail,
                    "bio": profileStore.data.bio,
                    "date_of_birth": profileStore.data.dateOfBirth,
                    "gender": profileStore.data.gender,
                    "fitness": profileStore.data.fitnessLevel,
                    "passports":profileStore.data.passportCountries,
                }
                Api.editProfile(authenticationStore.methods.getUserId(), updatedProfile, authenticationStore.methods.getSessionId())

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