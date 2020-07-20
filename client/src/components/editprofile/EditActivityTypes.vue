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
        <br>
        <b-button style="float:right" type="is-primary" @click="submitActivityTypes">Save</b-button>
        <br>
    </div>
</template>

<script>
    import List from "../List";
    import Api from "../../Api";
    import toastMixin from "../../mixins/toastMixin";

    export default {
        name: "EditActivityTypes",
        mixins: [toastMixin],
        components: {List},
        props: ["profile"],
        data(){
            return {
                possibleActivityTypes: [],
                newActivityType: "",
                chosenActivityTypes: this.profile.activities,
                originalActivityTypes: this.profile.activities
            }
        },
        methods: {

            getAllActivityTypes(){
                Api.getActivityTypesList().then(response => this.possibleActivityTypes = response.data.allActivityTypes)
            },

            deleteActivityType(chosenActivityType) {
                this.chosenActivityTypes = this.chosenActivityTypes.filter(activityType => activityType != chosenActivityType)
            },
            addActivityType() {
                if (this.newActivityType === ""){
                    this.warningToast("No activity selected")
                } else if (this.chosenActivityTypes.includes(this.newActivityType)) {
                    this.warningToast("Activity already in list")
                } else {
                    this.chosenActivityTypes = [...this.chosenActivityTypes, this.newActivityType]
                }
            },
            submitActivityTypes() {
                if (this.originalActivityTypes === this.chosenActivityTypes) {
                    this.warningToast("No changes made")
                } else {
                    this.$parent.updateActivityTypes(this.chosenActivityTypes)
                    this.successToast("New activity types saved")
                    this.originalActivityTypes = this.chosenActivityTypes
                }
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