<template>
    <div class="card">
        <div class="columns">
            <div class="column">
                <h4><strong>{{activity.activityName}}</strong></h4>

                <p>{{activity.creatorName}}</p>
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
                        <b-dropdown-item aria-role="listitem" @click="goToActivity(activity)">View activity</b-dropdown-item>
                        <b-dropdown-item v-if="store.getters.getAuthenticationLevel <= 1" aria-role="listitem" @click="editActivity(activity)">Edit activity </b-dropdown-item>
                        <b-dropdown-item v-if="store.getters.getAuthenticationLevel <= 1" aria-role="listitem" @click="deleteActivity(activity.id)">Delete activity</b-dropdown-item>
                    </b-dropdown>
                </template>
            </b-menu-item>
        </div>
    </div>
</template>

<script>
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
        props: {
            activity: {
                type: Object,
                required: true
            }
        },
        mounted() {
            if (this.props != undefined) {
                this.activityData = this.props.activity;
            }

        },
        methods: {
            editActivity(activity) {
                Api.getActivity(activity.id, localStorage.getItem("authToken"))
                    .then((response) => {
                        let wholeActivity = response.data;
                        router.push({name: 'editActivity', params: {activityProp: wholeActivity}});
                    })
                    .catch(error => console.log(error));

            },
            goToActivity(activity) {
                router.push({path: 'Activities/' + activity.id})
            },
            deleteActivity(id) {
                Api.deleteActivity(store.getters.getUserId, localStorage.getItem('authToken'), id)
                    .then(() => {
                        this.$parent.removeActivityFromList(id);
                        this.warningToast("Activity deleted");
                    })
                    .catch(error => console.log(error));
            },
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