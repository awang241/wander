<template>
    <div class="container">
        <h4 class="title is-5">Add Activities</h4>
        <b-field>
            <b-select placeholder="Select an activity" v-model="newActivity" expanded>
                <option
                        v-for="activity in possibleActivities"
                        :value="activity"
                        :key="activity">
                    {{ activity }}
                </option>
            </b-select>
            <b-button type="is-primary" @click="addActivity">Add</b-button>
        </b-field>
        <List v-bind:chosenItems="chosenActivities" v-on:deleteListItem="deleteActivity"></List>
        <b-button type="is-primary" @click="submitActivities">Save</b-button>
    </div>
</template>

<script>
    import List from "../List";
    import Api from "../../Api";
    import profileStore from "../../store/profileStore";
    import authenticationStore from "../../store/authenticationStore";

    export default {
        name: "EditActivities",
        components: {List},
        data(){
            return {
                possibleActivities: "",
                newActivity: "",
                chosenActivities: profileStore.data.activities,
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
            deleteActivity(chosenActivity) {
                this.chosenActivities = this.chosenActivities.filter(activity => activity != chosenActivity)
            },
            addActivity() {
                console.log("adding activity")
                console.log("chosenActivities: " + this.chosenActivities)
                if (this.newActivity === ""){
                    this.showWarning("No activity selected")
                } else if (this.chosenActivities.includes(this.newActivity)) {
                    this.showWarning("Activity already in list")
                } else {
                    this.chosenActivities = [...this.chosenActivities, this.newActivity]
                }
            },
            submitActivities() {
                profileStore.methods.setActivities(this.chosenActivities)
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
                    "activities": profileStore.data.activities
                }
                Api.editProfile(authenticationStore.methods.getUserId(), updatedProfile, authenticationStore.methods.getSessionId())

            }
        },
        mounted() {
            this.possibleActivities = profileStore.data.allActivities
            //this.chosenActivities = profileStore.data.activities
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