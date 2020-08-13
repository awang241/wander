<template>
    <div class="container containerColor">
        <h1 class="title">Are you sure you want to change this activity to <b>{{activityPrivacy}}?</b></h1>
        <h2 class="subtitle">This would remove these users access to the activity</h2>

        <h2><b>{{numFollowers}}</b> Followers</h2>
        <h2><b>{{numParticipants}}</b> Participants</h2>
        <h2><b>{{numOrganizers}}</b> Organizers</h2>

        <br>
        <div class="block">
            <b-checkbox v-model="rolesToRetain"
                        native-value="followers">
                Retain Followers
            </b-checkbox>
            <b-checkbox v-model="rolesToRetain"
                        native-value="participants">
                Retain Participants
            </b-checkbox>

            <b-checkbox v-model="rolesToRetain"
                        native-value="organizers">
                Retain Organizers
            </b-checkbox>
        </div>
        <b-button class="is-danger" @click="this.$parent.close">Cancel</b-button>
        <b-button class="is-primary is-pulled-right" @click="confirmPrivacyChange">Continue</b-button>
    </div>
</template>

<script>

    import Api from "../Api";

    export default {
        name: "ActivityShareConfirmation",
        props: {
            activityId: {
                type: Number,
                required: true
            },
            activityPrivacy: {
                type: String,
                required: true
            }
        },
        data(){
            return {
                numFollowers: 0,
                numOrganizers: 0,
                numParticipants: 0,
                rolesToRetain: []
            }
        },
        mounted() {
            this.getRoleCountsForActivity()
        },
        methods: {
            confirmPrivacyChange() {
                this.$emit('confirmPrivacyChange', this.rolesToRetain)
            },
            getRoleCountsForActivity(){
                Api.getRoleCountsForActivity(this.activityId, localStorage.getItem('authToken') )
                .then(response => {
                    this.numFollowers = response.data.followers
                    this.numOrganizers = response.data.organizers
                    this.numParticipants = response.data.participants
                })
            }
        }
    }
</script>

<style scoped>

</style>