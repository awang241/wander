<template>
    <div v-if="store.getters.getAuthenticationStatus" class="container containerColor">
        <b-tabs expanded>
            <b-tab-item label="Profile Search">
                <ProfileSearch viewing-as-admin="false">
                    <template #header>
                        <h1 class="title">Profile Search</h1>
                    </template>
                </ProfileSearch>
            </b-tab-item>
            <b-tab-item label="Activity Location">
                <ActivitySearchByLocation>
                </ActivitySearchByLocation>
            </b-tab-item>
            <b-tab-item label="Activity Search">
                <ActivitySearchByName>
                </ActivitySearchByName>
            </b-tab-item>
        </b-tabs>

    </div>
</template>

<script>
    import ProfileSearch from "./ProfileSearch";
    import ActivitySearchByLocation from "./ActivitySearchByLocation"
    import ActivitySearchByName from "./ActivitySearchByName"
    import store from "../../store/index"

    export default {
        name: "Search",
        components: {ProfileSearch, ActivitySearchByLocation, ActivitySearchByName},
        data() {
            return {
                store: store
            }
        }

    }
</script>

<style scoped>

</style>