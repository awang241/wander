<template>
    <div class="card">
        <div class="columns">
            <div class="column">
                <h4><strong>{{activity.activity_name}}</strong></h4>

                <p>{{activity.creator}}</p>
                <p>{{activity.location}}</p>
            </div>
            <div v-if="activity.activityTypes.length > 0" class="column">
                <strong>Activity Types</strong>
                <div v-for="activityType in activity.activityTypes" :key="activityType">
                    <p> {{activityType}}</p>
                </div>
            </div>
            <b-menu-item class="is-vertical-center">
                <template slot="label">
                    <b-dropdown aria-role="list" class="is-pulled-right" position="is-bottom-left">
                        <b-icon icon="ellipsis-v" slot="trigger"></b-icon>
                        <b-dropdown-item aria-role="listitem" @click="goToActivity">View activity</b-dropdown-item>
                        <b-dropdown-item v-if="store.getters.getAuthenticationLevel <= 1" aria-role="listitem" @click="editActivity">Edit activity </b-dropdown-item>
                        <b-dropdown-item v-if="store.getters.getAuthenticationLevel <= 1" aria-role="listitem" @click="deleteActivity">Delete activity</b-dropdown-item>
                    </b-dropdown>
                </template>
            </b-menu-item>
        </div>
    </div>
</template>

<script>
    import Activity from "./Activity.vue";
    import Api from "../Api";
    import toastMixin from "../mixins/toastMixin";
    import store from "../store";
    import router from "../router";

    export default {
        name: "ActivitySummary",
        mixins: [toastMixin],
        data() {
            return {
                activityData: {},
                store: store
            }
        },
        mounted() {
            this.activityData = this.props.activity;
        },
        props: {
            activity: {
                type: Object,
                required: true
            }
        },
        methods: {
            goToActivity(activity) {
                router.push({name: 'editActivity', params: {activityProp: activity}})
            },
            deleteActivity(id) {
                Api.deleteActivity(store.getters.getUserId, localStorage.getItem('authToken'), id)
                    .then((response) => {
                        console.log(response);
                        this.warningToast("Activity deleted")
                        this.activities = this.activities.filter(activity => activity.id != id);
                    })
                    .catch(error => console.log(error));
            }
        }
    }
</script>

<style scoped>
    .columns {
        padding: 1rem;
    }
    li{
        list-style-type: none;
    }
    .is-vertical-center {
        display: flex;
        align-items: center;
        padding-right: 1rem;
    }

    .color-primary {
        color: #4099FF
    }
</style>