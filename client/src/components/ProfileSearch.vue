<template>
    <div class="container">
        <h1 class="title">User search</h1>
        <form @submit.prevent="searchUser">

            <b-field label="Name" expanded>
                <b-input type="text"
                         v-model="name"
                         placeholder="Name">
                </b-input>
            </b-field>

            <b-field label="Email" expanded>
                <b-input type="text"
                         v-model="email"
                         placeholder="Email">
                </b-input>
            </b-field>

            <b-field label="Activity types">
                <b-taginput
                        v-model="chosenActivityTypes"
                        :data="filteredActivityTypes"
                        autocomplete
                        @typing="getFilteredActivityTypes"
                        :open-on-focus="false"
                        placeholder="Add an activity type">
                </b-taginput>
            </b-field>

            <div>
                <b-radio v-model="activitySearchType"
                         name="all types"
                         native-value="all">
                    Matching all types
                </b-radio>
                <b-radio v-model="activitySearchType"
                         name="any types"
                         native-value="any">
                    Matching any type
                </b-radio>
            </div>

            <br>

            <div class="column">
                <div class="is-pulled-left">
                    <b-button type="is-danger" @click="resetSearchFields">Reset fields</b-button>
                </div>
                <div class="is-pulled-right">
                    <b-field>
                        <b-button native-type="submit" class="is-primary">Search</b-button>
                    </b-field>
                </div>
            </div>

        </form>

        <div id="results" class="column" v-if="profiles.length">
            <div
                    v-for="profile in profiles"
                    :key="profile.id">
                <ProfileSummary :profile="profile"/>
            </div>
        </div>

        <div v-else id="noMatches">
            <h1>No profiles loaded!</h1>
        </div>

        <observer v-on:intersect="loadMoreProfiles"></observer>
    </div>
</template>

<script>
    import Api from "../Api";
    import router from "../router"
    import ProfileSummary from "./ProfileSummary";
    import Observer from "./Observer";

    const DEFAULT_RESULT_COUNT = 10

    export default {
        name: "ProfileSearch",
        components: {Observer, ProfileSummary},
        data() {
            return {
                activitySearchType: "all",
                chosenActivityTypes: [],
                email: "",
                name: "",
                possibleActivityTypes: [],
                filteredActivityTypes: this.possibleActivityTypes,
                observer: null,
                profiles: [],
                startIndex: 0
            }
        },
        mounted() {
            this.getPossibleActivityTypes()
        },
        methods: {
            getPossibleActivityTypes() {
                Api.getActivityTypesList()
                    .then(response => this.possibleActivityTypes = response.data.allActivityTypes)
                    .catch(error => this.showMessage(error))
            },
            resetSearchFields() {
                this.email = ""
                this.name = ""
                this.chosenActivityTypes = []
            },
            searchUser() {
                this.startIndex = 0
                const searchParameters = this.getSearchParameters()
                Api.getUserProfiles(localStorage.getItem('authToken'), searchParameters).then(response => {
                    this.startIndex += DEFAULT_RESULT_COUNT
                    this.profiles = response.data.results})
            },
            getSearchParameters() {
                const searchParameters = {count: DEFAULT_RESULT_COUNT, startIndex: this.startIndex}
                if (this.name.length !== 0) {
                    searchParameters.fullname = this.name
                }
                if (this.email.length !== 0) {
                    searchParameters.email = this.email
                }
                if (this.chosenActivityTypes.length > 0){
                    searchParameters.activityTypes = this.chosenActivityTypes.join(",")
                    searchParameters.method = this.activitySearchType
                }
                return searchParameters
            },
            //Autocomplete to display activity types that finish the word the user is typing
            getFilteredActivityTypes(text) {
                this.filteredActivityTypes = this.possibleActivityTypes.filter((option) => {
                    return option
                        .toString()
                        .toLowerCase()
                        .indexOf(text.toLowerCase()) >= 0
                })
            },
            openProfile() {
                router.push('/Profile')
            },
            loadMoreProfiles() {
                const searchParameters = this.getSearchParameters()
                Api.getUserProfiles(localStorage.getItem('authToken'), searchParameters).then(response => {
                    this.startIndex += DEFAULT_RESULT_COUNT
                    const profiles = response.data.results
                    this.profiles = [...this.profiles, ...profiles]
                })
            }
        }
    }
</script>

<style scoped>
    #results {
        padding-top: 4rem;
    }
    #noMatches{
        padding-top: 4rem;
        color: red;
    }
</style>
