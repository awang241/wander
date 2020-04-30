<template>
    <div class="container">
        <h4 class="title is-5">Add Activity Types</h4>
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

    export default {
        name: "EditActivityTypes",
        components: {List},
        props: ["profile"],
        data(){
            return {
                possibleActivityTypes: [],
                newActivityType: "",
                chosenActivityTypes: this.profile.activities,
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

            getAllActivityTypes(){
                Api.getActivityTypesList().then(response => this.possibleActivityTypes = response.data.allActivityTypes)
            },

            deleteActivityType(chosenActivityType) {
                this.chosenActivityTypes = this.chosenActivityTypes.filter(activityType => activityType != chosenActivityType)
            },
            addActivityType() {
                if (this.newActivityType === ""){
                    this.showWarning("No activity selected")
                } else if (this.chosenActivityTypes.includes(this.newActivityType)) {
                    this.showWarning("Activity already in list")
                } else {
                    this.chosenActivityTypes = [...this.chosenActivityTypes, this.newActivityType]
                }
            },
            submitActivityTypes() {
                this.$parent.updateActivityTypes(this.chosenActivityTypes)
                this.$buefy.toast.open({
                    duration: 2000,
                    message: "Saved!",
                    type: 'is-success',
                    position: 'is-top'
                })
            }
        },
        mounted() {
            this.getAllActivityTypes()
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