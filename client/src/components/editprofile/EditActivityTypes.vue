<template>
    <div class="container">
        <h4 class="title is-5">Add ActivityTypes</h4>
        <b-field>
            <b-select placeholder="Select an activityType" v-model="newActivityType" expanded>
                <option
                        v-for="activityType in possibleActivityTypes"
                        :value="activityType"
                        :key="activityType">
                    {{ activityType }}
                </option>
            </b-select>
            <b-button type="is-primary" @click="addActivityType">Add</b-button>
        </b-field>
        <List v-bind:chosenItems="chosenActivityTypes" v-on:deleteListItem="deleteActivityType"></List>
        <b-button type="is-primary" @click="submitActivityTypes">Save</b-button>
    </div>
</template>

<script>
    import List from "../List";
    import Api from "../../Api";
    import profileStore from "../../store/profileStore";
    import authenticationStore from "../../store/authenticationStore";

    export default {
        name: "EditActivityTypes",
        components: {List},
        data(){
            return {
                possibleActivityTypes: "",
                newActivityType: "",
                chosenActivityTypes: profileStore.data.activityTypes,
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
            deleteActivityType(chosenActivityType) {
                this.chosenActivityTypes = this.chosenActivityTypes.filter(activityType => activityType != chosenActivityType)
            },
            addActivityType() {
                console.log("adding activityType")
                console.log("chosenActivityTypes: " + this.chosenActivityTypes)
                if (this.newActivityType === ""){
                    this.showWarning("No activityType selected")
                } else if (this.chosenActivityTypes.includes(this.newActivityType)) {
                    this.showWarning("ActivityType already in list")
                } else {
                    this.chosenActivityTypes = [...this.chosenActivityTypes, this.newActivityType]
                }
            },
            submitActivityTypes() {
                profileStore.methods.setActivityTypes(this.chosenActivityTypes)
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
                    "additional_email": profileStore.data.optionalEmails,
                    "activityTypes": profileStore.data.activityTypes
                }
                Api.editProfile(authenticationStore.methods.getUserId(), updatedProfile, authenticationStore.methods.getSessionId())

            }
        },
        mounted() {
            this.possibleActivityTypes = profileStore.data.allActivityTypes
            //this.chosenActivityTypes = profileStore.data.activityTypes
        }
    }
</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
        margin-top: 0px;
        padding: 0px;
    }

</style>