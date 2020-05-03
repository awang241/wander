<template>
    <div class="container containerColor">
        <!-- Header -->
        <section class="hero level">
            <div class=" hero-body level-item">
                <div class="container containerColor">
                    <h1 class="title is-1">
                        Activities
                    </h1>
                </div>

                <!-- redirect to add activity -->
                <b-button
                           type="is-info">
                    Add Activity
                </b-button>
            </div>
        </section>

        <hr class ="hrLine">

        <!-- this is a placeholder, please remove after testing -->
        <div class="container containerColor has-same-height is-gapless">
            <div class="column">
                <!-- Activities -->
                <div class="card">
                    <div class="card-content">
                        <h3 class="title is-4">Name</h3>
                        role:
                        <div class="content">
                            <table class="table-profile">
                                <tr>
                                    <th colspan="1"></th>
                                    <th colspan="2"></th>
                                </tr>
                                <tr>
                                    <td>Description:</td>
                                    <td>description</td>
                                </tr>
                                <tr>
                                    <td>Continous/Duration:</td>
                                    <td>continuous</td>
                                </tr>
                                <tr>
                                    <td>Start Time:</td>
                                    <td>time</td>
                                </tr>
                                <tr>
                                    <td>End Time:</td>
                                    <td>time</td>
                                </tr>
                                <tr>
                                    <td>Location:</td>
                                    <td>time</td>
                                </tr>
                            </table>
                        </div>
                        <br>
                    </div>
                </div>
            </div>
        </div>

        <div v-for="activity in activities" v-bind:key="activity">
            <div class="container containerColor has-same-height is-gapless">
                <div class="column">
                    <!-- Activities -->
                    <div class="card">
                        <div class="card-content">
                            <h3 class="title is-4">Name</h3>
                            Role:
                            <div class="content">
                                <table class="table-profile">
                                    <tr>
                                        <th colspan="1"></th>
                                        <th colspan="2"></th>
                                    </tr>
                                    <tr>
                                        <td>Description:</td>
                                        <td>{{activity.description}}</td>
                                    </tr>
                                    <tr>
                                        <td>Continous/Duration:</td>
                                        <td>{{activity.continuous}}</td>
                                    </tr>
                                    <tr>
                                        <td>Start Time:</td>
                                        <td>{{activity.startTime}}</td>
                                    </tr>
                                    <tr>
                                        <td>End Time:</td>
                                        <td>{{activity.endTime}}</td>
                                    </tr>
                                    <tr>
                                        <td>Location:</td>
                                        <td>{{activity.location}}</td>
                                    </tr>
                                </table>
                            </div>
                            <br>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>

<script>
    import api from '../Api';
    export default {
        name: "Activities",
        data() {
            return {
                activities: api.getActivitiesList(),
            }
        },
        methods: {

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
        border:2px solid #EDEEEE;
    }

</style>