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
                <b-input type="email"
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
                        <b-button native-type="submit" class="is-primary">Submit</b-button>
                    </b-field>
                </div>
            </div>

        </form>

        <div id="results" class="column">
            <div
                 v-for="profile in profiles"
                 :key="profile.id">
                <ProfileSummary :profile="profile"/>
            </div>
        </div>

        <observer v-on:intersect="loadMoreProfiles"></observer>
    </div>
</template>

<script>
    import Api from "../Api";
    import router from "../router"
    import ProfileSummary from "./ProfileSummary";
    import Observer from "./Observer";

    const DEFAULT_COUNT = 10

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
                page: 1
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
                //TODO Implement this function to make api call to search for user based on values in search form
                Api.getUserProfiles(localStorage.getItem('authToken'), {count: DEFAULT_COUNT, startIndex: 0}).then(response =>
                    this.profiles = response.data.results)

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
            loadMoreProfiles(){
                //TODO uncomment this method and use correct api call to work with backend implementation
                // api.getProfiles(store.getters.getUserId, localStorage.getItem('authToken'))
                //     .then((response) => {
                //         this.page += 1
                //         const profiles = response.data;
                //         this.profiles = [...this.profiles, ...profiles]
                //     })
            }
        }
    }
</script>

<style scoped>
    #results{
        padding-top: 4rem;
    }
</style>