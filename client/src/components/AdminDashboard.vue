<template>
    <ProfileSearch viewing-as-admin="true">
        <template #header>
            <h1 class="title">Admin dashboard</h1>
        </template>
    </ProfileSearch>
</template>

<script>
    import ProfileSearch from "./ProfileSearch";

    export default {
        name: "AdminDashboard",
        components: {ProfileSearch},
    }
</script>