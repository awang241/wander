<template>

    <div class="container">

        <h1 class="Title">Change Password</h1>

        <form @submit="sendUpdatedData">
            <b-field label="Current Password" expanded >
                <b-input v-model="currentPassword" type="password" placeholder="Current Password" required></b-input>
            </b-field>
            <b-field label="New Password" expanded >
            <b-input v-model="newPassword" type="password" placeholder="New Password" required></b-input>
            </b-field>
            <b-field label="Confirm Password" expanded >
                <b-input v-model="confirmPassword" type="password" placeholder="Confirm Password" required></b-input>
            </b-field>
            <b-field>
                <b-button native-type="submit">Save</b-button>
            </b-field>
        </form>
    </div>

</template>

<script>
    export default {
        name: "EditPassword"
    }
</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
    }

</style>