<template>
    <div class="container containerColor">

        <div class="has-same-height is-gapless">
            <div v-if="activities.length">

                <div v-for="activity in publicActivities" v-bind:key="activity">
                    <div class="column">
                        <!-- Activities -->
                        <div class="card">
                            <div class="card-content">
                                <h3 class="title is-4">{{activity.activity_name}}</h3>
                                Role: CREATOR
                                <div class="content">
                                    <table class="table-profile">
                                        <caption hidden>Displayed Activity Table</caption>
                                        <tr>
                                            <th colspan="1" scope="col"></th>
                                            <th colspan="2" scope="col"></th>
                                        </tr>
                                        <tr>
                                            <td>Description:</td>
                                            <td>{{activity.description}}</td>
                                        </tr>
                                        <tr>
                                            <td>Continous/Duration:</td>
                                            <td v-if="activity.continuous">continuous</td>
                                            <td v-else>duration</td>
                                        </tr>

                                        <tr v-if="!activity.continuous">
                                            <td>Start Time:</td>
                                            <td>UTC {{dateFormat(activity.start_time)}}</td>
                                        </tr>
                                        <tr v-if="!activity.continuous">
                                            <td>End Time:</td>
                                            <td>UTC {{dateFormat(activity.end_time)}}</td>
                                        </tr>

                                        <tr>
                                            <td>Location:</td>
                                            <td>{{activity.location}}</td>
                                        </tr>
                                        <tr v-for="type in activity.activity_type" :key="type">
                                            <td>Activity Type:</td>
                                            <td>{{type}}</td>
                                        </tr>
                                    </table>
                                    <!--Placeholder buttons :)     -->
                                    <b-button class='px-3' id="followButton" @click="joinActivity(activity)"
                                              type="is-warning">
                                        Follow
                                    </b-button>
                                </div>
                                <br>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div v-else class="box">
                <h1>No activities to show :(</h1>
            </div>
        </div>
    </div>
</template>

<script>
    import store from "../../store";
    import toastMixin from "../../mixins/toastMixin";

    export default {
        name: "DiscoverActivities",
        props: ["activities"],
        mixins: [toastMixin],
        data() {
        return {
            publicActivities: this.activities,
            store: store
        }
    }
    }

</script>

<style scoped>
    .bannerColor {
        background-color: #64C6E3
    }

    .containerColor {
        background-color: #F7F8F9
    }

    .center {
        text-align: center;
    }

    .hrLine {
        border: 2px solid #EDEEEE;
    }

    #editButton {
        margin-left: 1rem;
    }

    #activities-key-info{
        display: flex;
        justify-content: space-between;
        padding: 0rem 1rem;
    }

</style>