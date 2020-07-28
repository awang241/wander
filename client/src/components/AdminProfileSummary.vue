<!--Component to hold the view of a users profile that an admin would see-->
<template>
    <h1>Admin component here</h1>
</template>

<script>
    export default {
        name: "AdminProfileSummary",
        props: {
            profile: {
                type: Object,
                required: true
            }
        }
    }
</script>

<style scoped>

</style>