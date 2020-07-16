<template>
    <div class="card">
        <div class="columns">
            <div class="column">
                <h4><strong>{{name}}</strong></h4>
                <p>{{profile.gender}}</p>
                <p>{{profile.email}}</p>
            </div>
            <div v-if="profile.activities.length > 0" class="column">
                <strong>Activities</strong>
                <div v-for="activity in profile.activities" :key="activity">
                    <p> {{activity}}</p>
                </div>
            </div>
            <b-button type="is-text" @click="gotoProfile(profile.id)">View profile</b-button>
        </div>
    </div>
</template>

<script>
    import router from "../router";

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
                return `${this.profile.firstname}  ${this.profile.lastname}`
            }
        },
        props: {
            profile: Object
        },
        methods: {
            gotoProfile(profileID) {
                window.scrollTo(0,0);
                router.push('Profile/' + profileID);
            }
        }
    }
</script>

<style scoped>
    .columns{
        padding: 1rem;
    }
</style>