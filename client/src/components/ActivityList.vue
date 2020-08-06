<template>
    <div v-if="store.getters.getAuthenticationStatus" class="container containerColor">
        <br>
        <br>
        <div class="has-same-height is-gapless">
            <div v-if="activities.length">
                <div v-for="activity in activities" v-bind:key="activity">
                    <SingleActivity v-bind:activity="activity"></SingleActivity>
                </div>
            </div>
            <div v-else class="box">
                <h1>No activities created</h1>
            </div>
        </div>
    </div>
</template>

<script>
    import store from "../store";
    import toastMixin from "../mixins/toastMixin";
    import SingleActivity from "./SingleActivity";

    export default {
        name: "ActivityList",
        props: ["activities"],
        components: {
            SingleActivity
        },
        mixins: [toastMixin],
        data() {
            return {
                store: store
            }
        }
    }
</script>

<style scoped>

</style>