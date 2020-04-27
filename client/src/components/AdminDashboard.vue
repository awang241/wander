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
                <td>{{ profile[0] }}</td>
                <td>{{ profile[1] }}</td>
                <td>{{ profile[2] }}</td>
                <td>{{ profile[4] }}</td>
                <td>{{ profile[3] }}</td>
                <td>"button to view profile"</td>
            </tr>
        </table>

    </div>
</template>

<script>
    // import api from '../Api';
    export default {
        name: "AdminDashboard",
        data() {
            return {
                allProfiles: {},
            }
        },
        methods: {
            // getProfilesForAdmin(){
            //     api.getAdminProfiles()
            //         .then((response) => {
            //             this.allProfiles = response.data;
            //         })
            //         .catch(error => console.log(error));
            // }
        },
        mounted() {
            // this.getProfilesForAdmin();

            // MOCK DATA - will be replaced by real data when api endpoint has been created
            this.allProfiles = [[null, "Steven", "Stevenson", "steven@steven.com", "male"], [1, "Bob", "Joe", "bobby@jo.com", "male"]];
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