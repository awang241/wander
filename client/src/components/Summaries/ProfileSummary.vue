<template>
    <div class="card">
        <div class="columns">
            <div class="column">
                <h4><strong>{{name}}</strong></h4>
                <div v-if="this.profileIsAdmin" class="color-primary">Admin</div>

                <p>{{profile.gender}}</p>
                <p>{{profile.email}}</p>
            </div>
            <div v-if="profile.activities.length > 0" class="column">
                <strong>Activity Types</strong>
                <div v-for="activity in profile.activities" :key="activity">
                    <p> {{activity}}</p>
                </div>
            </div>
            <slot name="options">
            </slot>
        </div>
    </div>
</template>

<script>

    import toastMixin from "../../mixins/toastMixin";
    import store from "../../store";

    export default {
        name: "ProfileSummary",
        mixins: [toastMixin],
        data() {
            return {
                profileData: {},
                store: store
            }
        },
        mounted() {
            this.profileData = this.$props.profile;
        },
        computed: {
            name() {
                return `${this.profile.firstname}  ${this.profile.lastname}`
            },
            profileIsAdmin(){
                return this.profile.authLevel < 2
            }
        },
        props: {
            profile: {
                type: Object,
                required: true
            }
        },
    }
</script>

<style scoped>
    .columns {
        padding: 1rem;
    }
    li{
        list-style-type: none;
    }

    .color-primary {
        color: #4099FF
    }
</style>