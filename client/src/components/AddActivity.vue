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
                        <b-datepicker
                                editable
                                :use-html5-validation="false"
                                placeholder="Select start date"
                                :date-formatter="dateFormatter"
                                :min-date="minDate"
                                v-model="startDate"
                                type="date" required
                                validation-message="Please enter a valid date"

                                pattern="^(?:(?:31(\/|-|\.)(?:0?[13578]|1[02]))\1|(?:(?:29|30)(\/|-|\.)(?:0?[13-9]|1[0-2])\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:29(\/|-|\.)0?2\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\d|2[0-8])(\/|-|\.)(?:(?:0?[1-9])|(?:1[0-2]))\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$">
                            >
                        </b-datepicker>
                    </b-field>

                    <b-field label="End date" expanded>
                        <b-datepicker
                                editable
                                :use-html5-validation="false"
                                placeholder="Select end date"
                                :date-formatter="dateFormatter"
                                :min-date="startDate"
                                v-model="endDate"
                                type="date" required
                                validation-message="Please enter a valid date"
                                pattern="^(?:(?:31(\/|-|\.)(?:0?[13578]|1[02]))\1|(?:(?:29|30)(\/|-|\.)(?:0?[13-9]|1[0-2])\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:29(\/|-|\.)0?2\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\d|2[0-8])(\/|-|\.)(?:(?:0?[1-9])|(?:1[0-2]))\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$">
                            >
                        </b-datepicker>
                    </b-field>
                </b-field>
                <b-field group-multiline grouped>
                    <b-field label="Start time" expanded>
                        <b-timepicker
                                v-model="startTime"
                                icon="clock">
                        </b-timepicker>
                    </b-field>

                    <b-field label="End time" expanded>
                        <b-timepicker
                                v-model="endTime"
                                icon="clock">
                        </b-timepicker>
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
                <b-button v-if="isValidActivity" native-type="submit" :enabled="isValidActivity">Submit</b-button>
            </b-field>
        </form>
    </div>

</template>


<script>
    import List from "./List";
    import Api from "../Api"

    export default {
        name: "AddActivity",
        components: {List},
        computed: {
            // a computed getter as radio buttons cannot return boolean values
            isContinuous: function () {
                return this.activityDuration === "Continuous"
            },
            isValidActivity: function(){
                return this.chosenActivityTypes.length > 0;
            }
        },
        data() {
            const today = new Date()
            return {
                name: "",
                description: "",
                startDate: new Date(),
                activityDuration: "Continuous",
                possibleActivityTypes: ["Running", "Cycling", "Basketball", "Karate"],
                chosenActivityTypes: [],
                newActivityType: "",
                endDate: new Date(),
                startTime: "",
                endTime: "",
                location: "",
                minDate: new Date(today.getFullYear(), today.getMonth(), today.getDate()),
            }
        },
        mounted() {
            this.getPossibleActivityTypes()
            // let today = new Date().toISOString().split('T')[0];
            // this.$refs.dateOfBirth.setAttribute('max', today);
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
            getPossibleActivityTypes(){
                Api.getPossibleActivityTypes()
                    .then(response => this.possibleActivityTypes = response.data)
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
