<template>
    <div class="container containerColor">
        <h1 class="title">Admin Dashboard</h1>
        <h2 class="title">All Users:</h2>

        <table>
            <tr>
                <th>Profile ID</th>
                <th>First Name</th>
                <th>Last Name</th>
                <th>Primary Email</th>
            </tr>
            <tr v-for="profile in allProfiles" v-bind:key="profile">
                <td>{{ profile.id }}</td>
                <td>{{ profile.fname }}</td>
                <td>{{ profile.lname }}</td>
                <td>{{ profile.email }}</td>
            </tr>
        </table>

    </div>
</template>

<script>
    import api from '../Api';
    export default {
        name: "AdminDashboard",
        data() {
            return {
                allProfiles: {}
            }
        },
        mounted() {
            api.getAdminProfiles()
                .then((response) => {
                    this.allProfiles = response;

                })
                .catch(error => console.log(error));
        }
    }
</script>

<style scoped>
    .containerColor {
        background-color: #F7F8F9
    }
</style>