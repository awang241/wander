<template>
    <div v-if="store.getters.getAuthenticationStatus" class="container containerColor">
        <div id="results" class="column" v-if="activities.length">
            <div
                    v-for="activity in activities"
                    :key="activity.id">
                <ActivitySummary :activity="activity">
                </ActivitySummary>
                <br>
            </div>
        </div>
        <div v-else id="noMatches">
            <h1>No activities fetched!</h1>
        </div>
        <observer v-on:intersect="$emit('loadMoreActivities')"/>
    </div>
</template>

<script>
    import store from "../store";
    import toastMixin from "../mixins/toastMixin";
    import ActivitySummary from "./ActivitySummary";
    import Observer from "./Observer";

    export default {
        name: "ActivityList",
        props: ["activities", "role"],
        components: {
            Observer,
            ActivitySummary
        },
        mixins: [toastMixin],
        data() {
            return {
                store: store,
                observer: null
            }
        },
        methods: {
            loadMoreActivities() {
                this.$parent.loadMoreActivities(this.role)
            },
            removeActivityFromList(activityId) {
                this.$parent.removeActivityFromList(activityId);
            }
        }
    }
</script>

<style scoped>

</style>