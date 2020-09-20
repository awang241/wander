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
            <b-tab-item label="Activity Search">
                <ActivitySearch>
                </ActivitySearch>
            </b-tab-item>
        </b-tabs>

    </div>
</template>

<script>
    import ProfileSearch from "./ProfileSearch";
    import ActivitySearch from"./ActivitySearch"
    import store from "../../store/index"

    export default {
        name: "Search",
        components: {ProfileSearch, ActivitySearch},
        data() {
            return {
                store: store
            }
        }

    }
</script>

<style scoped>

</style>