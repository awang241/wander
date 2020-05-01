<template>
    <div class="container">
        <div class="container">
            <h1 class="title is-1">Activities</h1>
        </div>
        <b-button type="is-info">
            Add Activity
        </b-button>
        <div class="card">
            <div class="container">
                <h3 class="title is-5">Activity Name</h3>
                <div class="content">
                    <table class="content">

                    </table>
                </div>
            </div>
        </div>
    </div>
</template>

<script>
    export default {
        name: "Activities"
    }
</script>

<style scoped>
    b-button {
        color: green;
    }

</style>