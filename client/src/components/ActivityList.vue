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
        <observer v-on:intersect="loadMoreActivities"></observer>
    </div>
</template>

<script>
    import store from "../store";
    import toastMixin from "../mixins/toastMixin";
    import ActivitySummary from "./ActivitySummary";
    import Activities from "./Activities";

    export default {
        name: "ActivityList",
        props: ["activities", "role"],
        components: {
            ActivitySummary
        },
        mixins: [toastMixin],
        data() {
            return {
                store: store
            }
        },
        methods: {
            loadMoreActivities() {
                Activities.methods.loadMoreActivities(this.role);
            }
        }
    }
</script>

<style scoped>

</style>