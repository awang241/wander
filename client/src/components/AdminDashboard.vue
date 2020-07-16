<template>
    <div class="container containerColor">
        <h1 class="title is-3">Admin Dashboard</h1>
        <br>
        <h2 class="title is-5">All Users:</h2>
        <table>
            <caption hidden>Table of All Users</caption>
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
                <td><b-button type="is-text" native-type="submit" @click="goToProfile(profile.id)">Edit Profile</b-button></td>
            </tr>
        </table>
    <observer v-on:intersect="loadMoreProfiles"></observer>
    </div>
</template>

<script>
    import api from '../Api';
    import router from "../router";
    import Observer from "./Observer";

    const DEFAULT_RESULT_COUNT = 10

    export default {
        name: "AdminDashboard",
        components: {Observer},
        data() {
            return {
                allProfiles: [],
                startIndex: 0
            }
        },
        methods: {
            getProfilesForAdmin() {
                api.getUserProfiles(localStorage.getItem('authToken'), {startIndex: 0, count: DEFAULT_RESULT_COUNT})
                    .then((response) => {
                        this.allProfiles = response.data.results;
                    })
                    .catch(error => console.log(error));
            },
            goToProfile(profileID) {
                router.push('EditProfile/' + profileID);
            },
            loadMoreProfiles() {
                api.getUserProfiles(localStorage.getItem('authToken'), {
                    count: DEFAULT_RESULT_COUNT,
                    startIndex: this.startIndex
                }).then(response => {
                    this.startIndex += DEFAULT_RESULT_COUNT
                    const profiles = response.data.results
                    this.allProfiles = [...this.allProfiles, ...profiles]
                })
            }
        },
        mounted() {
            this.getProfilesForAdmin();
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