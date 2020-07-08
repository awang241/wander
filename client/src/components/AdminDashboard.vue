<template>
    <div class="container containerColor">
        <h1 class="title is-3">Admin Dashboard</h1>
        <br>
        <h2 class="title is-5">All Users:</h2>
        <table summary="List of all profiles">
            <tr>
                <th scope="col">Profile ID</th>
                <th scope="col">First Name</th>
                <th scope="col">Last Name</th>
                <th scope="col">Gender</th>
                <th scope="col">Primary Email</th>
                <th scope="col">View Profile</th>
            </tr>
            <tr v-for="profile in allProfiles" v-bind:key="profile">
                <td>{{ profile.id }}</td>
                <td>{{ profile.firstname }}</td>
                <td>{{ profile.lastname }}</td>
                <td>{{ profile.gender }}</td>
                <td>{{ profile.email }}</td>
                <td><b-button type="is-info" native-type="submit" @click="goToProfile(profile.id)">Edit Profile</b-button></td>
            </tr>
        </table>




    </div>
</template>

<script>
    import api from '../Api';
    import router from "../router";
    export default {
        name: "AdminDashboard",
        components: {

        },
        data() {
            return {
                allProfiles: null,
            }
        },
        methods: {
            getProfilesForAdmin(){
                api.getUserProfiles(localStorage.getItem('authToken'))
                    .then((response) => {
                        this.allProfiles = response.data;
                    })
                    .catch(error => console.log(error));
            },
            goToProfile(profileID){
                router.push('EditProfile/' + profileID);
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
        padding: 10px;
    }

</style>