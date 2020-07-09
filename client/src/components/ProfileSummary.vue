<template>
    <div class="card">
        <div class="columns">
            <div class="column">
                <h4><strong>{{this.name}}</strong></h4>
                <p>{{profileData.gender}}</p>
            </div>
            <div v-for="activity in profileData.activityTypes" :key="activity.id">
                <label>{{activity.activity_name}}</label>
            </div>
        </div>
    </div>
</template>

<script>
    export default {
        name: "ProfileSummary",
        data() {
            return {
                profileData: {}
            }
        },
        mounted() {
            this.profileData = this.props.profile;
        },
        computed: {
            name() {
                return this.profileData.firstname + this.profileData.lastname;
            }
        },
        props: {
            profile: Object
        }
    }
</script>

<style scoped>

</style>