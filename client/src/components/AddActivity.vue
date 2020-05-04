<template>
    <div class="container">
        <h1 class="title">Create Activity</h1>
        <form @submit.prevent="createActivity">

            <b-field label="Activity Name" expanded>
                <b-input v-model="name" placeholder="Enter activity name here" required></b-input>
            </b-field>

            <b-field label="Description" expanded>
                <b-input v-model="description" maxlength="500" type="textarea"
                         placeholder="Enter a description" required></b-input>
            </b-field>

            <div class="block">
                <b-field label="Activity duration" expanded></b-field>
                <b-radio v-model="activityDuration"
                         name="name"
                         native-value="Continuous">
                    Continuous
                </b-radio>
                <b-radio v-model="activityDuration"
                         name="name"
                         native-value="Duration">
                    Duration
                </b-radio>
            </div>


            <div v-if="!isContinuous">
                <b-field group-multiline grouped>
                    <b-field label="Start date" expanded>
                        <input class="input" type="date" v-model="startDate">
                    </b-field>
                    <b-field label="End date" expanded>
                        <input class="input" type="date" v-model="endDate">
                    </b-field>
                </b-field>
                <b-field group-multiline grouped>
                    <b-field label="Start time" expanded>
                        <input class="input" type="time" v-model="startTime">
                    </b-field>
                    <b-field label="End time" expanded>
                        <input class="input" type="time" v-model="endTime">
                    </b-field>
                </b-field>
                <br>
            </div>

            <b-field label="Activity location" expanded>
                <b-input v-model="location" placeholder="Enter activity location" required></b-input>
            </b-field>

            <h4 class="label">Add at least one activity type</h4>
            <b-field>
                <b-select placeholder="Select at least one activity type" v-model="newActivityType" expanded>
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

            <br>

            <b-field>
                <b-button native-type="submit">Submit</b-button>
            </b-field>
        </form>
    </div>

</template>


<script>
    import List from "./List";
    import Api from "../Api"
    import store from "../store";

    export default {
        name: "AddActivity",
        components: {List},
        computed: {
            // a computed getter as radio buttons cannot return boolean values
            isContinuous: function () {
                return this.activityDuration === "Continuous"
            },
            combinedStartDate: function () {
                if(this.startDate === null){
                    return null
                }
                let dateParts = this.startDate.split('-')
                let timeParts = this.startTime.split(':')

                if (dateParts && timeParts) {
                    dateParts[1] -= 1;
                    return new Date(Date.UTC.apply(undefined, dateParts.concat(timeParts))).toISOString();
                }
                return null;
            },
            combinedEndDate: function () {
                if(this.endDate === null){
                    return null
                }
                let dateParts = this.endDate.split('-')
                let timeParts = this.endTime.split(':')

                if (dateParts && timeParts) {
                    dateParts[1] -= 1;
                    return new Date(Date.UTC.apply(undefined, dateParts.concat(timeParts))).toISOString();
                }
                return null;
            }
        },
        data() {
            return {
                name: "",
                description: "",
                startDate: null,
                activityDuration: "Continuous",
                possibleActivityTypes: [],
                chosenActivityTypes: [],
                newActivityType: "",
                endDate: null,
                startTime: "",
                endTime: "",
                location: "",
            }
        },
        mounted() {
            this.getPossibleActivityTypes()
        },
        methods: {
            showWarning(errorStatusCode) {
                const message = errorStatusCode
                this.$buefy.toast.open({
                    duration: 5500,
                    message: message,
                    type: 'is-danger',
                    position: 'is-top'
                })
            },
            getPossibleActivityTypes() {
                Api.getActivityTypesList()
                    .then(response => this.possibleActivityTypes = response.data.allActivityTypes)
                    .catch(error => this.showMessage(error))
            },
            addActivityType() {
                if (this.newActivityType === "") {
                    this.showWarning("No Activity type selected")
                } else if (this.chosenActivityTypes.includes(this.newActivityType)) {
                    this.showWarning("Activity type already in list")
                } else {
                    this.chosenActivityTypes = [...this.chosenActivityTypes, this.newActivityType]
                }
            },
            showMessage(message) {
                this.$buefy.toast.open({
                    duration: 5500,
                    message: message,
                    type: 'is-success',
                    position: 'is-top'
                })
            },
            dateFormatter(dt) {
                return dt.toLocaleDateString('en-GB', {year: 'numeric', month: 'numeric', day: 'numeric'});
            },
            deleteActivityType(typeToDelete) {
                this.chosenActivityTypes = this.chosenActivityTypes.filter(type => type != typeToDelete)
            },
            validateActivity(){
                let isValid = true;
                if (this.chosenActivityTypes.length < 1){
                    this.showMessage("You must choose at least one activity type")
                    isValid = false
                } else if (!this.isContinuous) {
                    const startDate = Date.parse(this.combinedStartDate)
                    const endDate = Date.parse(this.combinedEndDate)
                    if (isNaN(startDate) || isNaN(endDate)) {
                        this.showMessage("Invalid dates entered!")
                        isValid = false
                    } else if (Date.parse(this.combinedStartDate) > Date.parse(this.combinedEndDate)) {
                        this.showMessage("The end date must be after the start date")
                        isValid = false
                    }
                }
                return isValid
            }, createActivity() {
                if(this.validateActivity()){
                    let activity = {
                        "activity_name": this.name,
                        "description": this.description,
                        "activity_type": this.activityDuration,
                        "location": this.location,
                    }
                    if (!this.isContinuous) {
                        activity.start_time = this.combinedStartDate
                        activity.end_time = this.combinedEndDate
                    }
                    this.submitActivity(activity)
                }
            }, submitActivity(activity){
                Api.createActivity(store.getters.getUserId(), activity)
            }
        }
    }
</script>


<style scoped>
    .container {
        width: 800px;
    }

    @media only screen and (max-width: 600px) {
        .container {
            width: 100%;
        }
    }

</style>
