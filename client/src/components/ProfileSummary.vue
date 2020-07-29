<template>
    <div class="card">
        <div class="columns">
            <div class="column">
                <h4><strong>{{name}}</strong></h4>
                <div v-if="profile.authLevel < 2" class="color-primary">Admin</div>

                <p>{{profile.gender}}</p>
                <p>{{profile.email}}</p>
            </div>
            <div v-if="profile.activities.length > 0" class="column">
                <strong>Activity Types</strong>
                <div v-for="activity in profile.activities" :key="activity">
                    <p> {{activity}}</p>
                </div>
            </div>
            <b-button type="is-text" @click="gotoProfile(profile.id)">View profile</b-button>
        </div>
    </div>
</template>

<script>
    import Profile from "./Profile.vue";

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
            profile: {
                type: Object,
                required: true
            }
        },
        methods: {
            gotoProfile() {
                this.$buefy.modal.open({
                    parent: this,
                    props: {id:this.profile.id},
                    component: Profile,
                    trapFocus: true,
                    scroll: "clip"
                })
            }
        }
    }
</script>

<style scoped>
    .columns{
        padding: 1rem;
    }
    .color-primary {
        color: #4099FF
    }
</style>