<template>
    <div class="card">
        <div class="columns">
            <div class="column">
                <h4><strong>{{activity.activityName}}</strong></h4>

                <p>{{activity.creatorName}}</p>
                <p>{{activity.location}}</p>
                <p v-if="activity.continuous">Continuous</p>
                <p v-else>Duration</p>

            </div>
            <div v-if="activity.activityTypes.length > 0" class="column">
                <strong>Activity Types</strong>
                <div v-for="activityType in activity.activityTypes" :key="activityType">
                    <p> {{activityType}}</p>
                </div>
            </div>
            <b-menu-item class="is-vertical-center">
                <template slot="label">
                    <b-dropdown v-if="store.getters.getAuthenticationLevel <= 1" aria-role="list" class="is-pulled-right" position="is-bottom-left">
                        <b-icon icon="ellipsis-v" slot="trigger"></b-icon>
                        <b-dropdown-item aria-role="listitem" @click="goToActivity(activity)">View activity</b-dropdown-item>
                        <b-dropdown-item aria-role="listitem" @click="editActivity(activity)">Edit activity </b-dropdown-item>
                    </b-dropdown>
                    <b-button v-else type="is-text" @click="goToActivity(activity)">View activity</b-button>
                </template>
            </b-menu-item>
        </div>
    </div>
</template>

<script>
    import Api from "../../Api";
    import toastMixin from "../../mixins/toastMixin";
    import store from "../../store";
    import router from "../../router";
    import dateTimeMixin from "../../mixins/dateTimeMixin";

    export default {
        name: "ActivitySummary",
        mixins: [toastMixin, dateTimeMixin],
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
                    .catch(() => this.warningToast("Error occurred while getting Activity details."));

            },
            goToActivity(activity) {
                router.push({path: 'Activities/' + activity.id})
            },
            dateFormat(date) {
                return this.dateFormat(date);
            }
        }
    }
</script>

<style scoped>
    .columns {
        padding-left: 1rem;
        padding-right: 1rem;
        padding-bottom: 1rem;
    }

    .is-vertical-center {
        display: flex;
        align-items: center;
        padding-right: 1rem;
    }

</style>