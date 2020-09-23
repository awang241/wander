<template>
    <div class="container">
        <h1 class="title">Activity Search</h1>
        <form @submit.prevent="searchActivity">
            <b-field group-multiline grouped>
                <b-field label="Activity Name" expanded>
                    <b-input type="text"
                             v-model="activityName"
                             placeholder="Activity Name">
                    </b-input>
                </b-field>
            </b-field>
            <br>
            <div>
                <b-radio v-model="activitySearchType"
                         name="all types"
                         native-value="all">
                    Matching all words
                </b-radio>
                <b-radio v-model="activitySearchType"
                         name="any types"
                         native-value="any">
                    Matching any word
                </b-radio>
            </div>
            <div class="column">
                <div class="is-pulled-right">
                    <b-field>
                        <b-button native-type="submit" class="is-primary" @click="search()">Search</b-button>
                    </b-field>
                </div>
            </div>
        </form>
        <br>
        <hr>

        <div id="results" v-if="activityResults.length">
            <h1><b>Activities returned from Search:</b></h1>
            <br>
            <div style="overflow-y: auto; overflow-x: hidden">
                <div
                        v-for="activity in activityResults"
                        :key="activity.id">
                    <ActivitySummary :activity="activity">
                    </ActivitySummary>
                    <br>
                </div>
            </div>

        </div>
        <div v-else id="noMatches">
            <h1><b>{{searchResultString}}</b></h1>
        </div>

    </div>


</template>

<script>
    import ActivitySummary from '../Summaries/ActivitySummary';
    import Api from "../../Api";

    const DEFAULT_RESULT_COUNT = 10

    export default {
        name: "ActivitySearchByName",
        components: {
            ActivitySummary
        },
        data() {
            return {
                activitySearchType: "all",
                activityResults: [],
                searchResultString: "Please click the 'Search' button below!",
                activityName: "",
                startIndex: 0,
                moreActivitiesExist: true,
            }
        },
        methods: {
            searchActivity() {
                this.startIndex = 0;
                const searchParameters = this.getSearchParameters();
                Api.getActivitiesByName(localStorage.getItem('authToken'), searchParameters).then(response => {
                    this.startIndex += DEFAULT_RESULT_COUNT;
                    this.activityResults = response.data.results
                    if (response.data.results.length == 0) {
                        this.searchResultString = "No activities found"
                    }
                })
            },
            getSearchParameters() {
                const searchParameters = {count: DEFAULT_RESULT_COUNT, startIndex: this.startIndex};
                if (this.activityName.length !== 0) {
                    searchParameters.name = this.activityName
                }
                searchParameters.method = this.activitySearchType
                return searchParameters
            },
            loadMoreActivities() {
                if (this.moreActivitiesExist) {
                    const searchParameters = this.getSearchParameters()
                    Api.getActivitiesByName(localStorage.getItem('authToken'), searchParameters).then(response => {
                        if (response.data.results.length === 0) {
                            this.moreActivitiesExist = false;
                        } else {
                            this.startIndex += DEFAULT_RESULT_COUNT
                            const activities = response.data.results
                            this.activityResults = [...this.activityResults, ...activities]
                        }
                    })
                }
            }
        }
    }
</script>