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
    </div>
</template>

<script>
    import Api from "../Api";

    export default {
        name: "ProfileSearch",
        data() {
            return {
                searchData: {},
                activitySearchType: "all",
                chosenActivityTypes: [],
                email: "",
                name: "",
                possibleActivityTypes: ['Hiking', 'Running', 'Swimming'],
                filteredActivityTypes: this.possibleActivityTypes
            }
        },
        mounted() {
            //this.getPossibleActivityTypes()
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
            },
            //Autocomplete to display activity types that finish the word the user is typing
            getFilteredActivityTypes(text) {
                this.filteredActivityTypes = this.possibleActivityTypes.filter((option) => {
                    return option
                        .toString()
                        .toLowerCase()
                        .indexOf(text.toLowerCase()) >= 0
                })
            }
        }


    }
</script>

<style scoped>

</style>