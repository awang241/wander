<template>
    <div class="container containerColor">
        <h1 class="title is-3">Admin Dashboard</h1>
        <br>
        <h2 class="subtitle is-5">All Users:</h2>
        <table>
            <tr>
                <th>Profile ID</th>
                <th>First Name</th>
                <th>Last Name</th>
                <th>Gender</th>
                <th>Primary Email</th>
                <th>View Profile</th>
            </tr>
            <tr v-for="profile in allProfiles" v-bind:key="profile">
                <td>{{ profile.id }}</td>
                <td>{{ profile.firstname }}</td>
                <td>{{ profile.lastname }}</td>
                <td>{{ profile.gender }}</td>
                <td>{{ profile.email }}</td>
                <td>"button to view profile"</td>
            </tr>
        </table>

    </div>
</template>

<script>
    import api from '../Api';
    import authenticationStore from "../store/authenticationStore";
    export default {
        name: "AdminDashboard",
        data() {
            return {
                allProfiles: null,
            }
        },
        methods: {
            getProfilesForAdmin(){
                api.getUserProfiles(authenticationStore.methods.getSessionId())
                    .then((response) => {
                        this.allProfiles = response.data;
                    })
                    .catch(error => console.log(error));
            }
        },
        mounted() {
            this.getProfilesForAdmin();

            // MOCK DATA - will be replaced by real data when api endpoint has been created
            // this.allProfiles = [[null, "Steven", "Stevenson", "steven@steven.com", "male"], [1, "Bob", "Joe", "bobby@jo.com", "male"]];
        }
    }
</script>

<style scoped>
    .containerColor {
        background-color: #F7F8F9
    }
    th {
        width: 150px;
    }
    table, th, td {
        border: 1px solid black;
        padding: 8px;
    }

</style>