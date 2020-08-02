<template>
    <div class="container containerColor">
        <!-- Header -->
        <section>
            <div id="activity-key-info">
                <div>
                    <b-button style="float:right" @click="shareActivity"
                              type="is-primary">
                        Share Activity
                    </b-button>
                    <h1 class="title is-1">
                        insert activity name here
                    </h1>
                    <h2 class="subtitle is-5">
                        Created by: creator name
                    </h2>
                    <div>
                        <h3 class="title is-5"> Privacy: privacy status</h3>
                    </div>
                </div>
                <div>
<!--                    <b-button v-if="creator/organiser"-->
<!--                              @click="editActivity"-->
<!--                              type="is-primary">-->
<!--                        Edit-->
<!--                    </b-button>-->
                </div>
            </div>
        </section>

        <div class="has-same-height is-gapless">
          <div>
                <div>
                    <div class="column">
                        <!-- Activities -->
                        <div class="card">
                            <div class="card-content">
                                <h3 class="title is-4">activity name</h3>
                                <div class="content">
                                    <table class="table-profile">
                                        <caption hidden>Displayed Activity Table</caption>
                                        <tr>
                                            <th colspan="1" scope="col"></th>
                                            <th colspan="2" scope="col"></th>
                                        </tr>
                                        <tr>
                                            <td>Description:</td>
<!--                                            <td>{{activity.description}}</td>-->
                                        </tr>
                                        <tr>
                                            <td>Continous/Duration:</td>
<!--                                            <td v-if="activity.continuous">continuous</td>-->
<!--                                            <td v-else>duration</td>-->
                                        </tr>

                                        <tr v-if="!activity.continuous">
                                            <td>Start Time:</td>
                                          <td>time</td>
<!--                                            <td>UTC {{dateFormat(activity.start_time)}}</td>-->
                                        </tr>
                                        <tr v-if="!activity.continuous">
                                            <td>End Time:</td>
                                          <td>time</td>
<!--                                            <td>UTC {{dateFormat(activity.end_time)}}</td>-->
                                        </tr>

                                        <tr>
                                            <td>Location:</td>
                                            <td>insert location</td>
                                        </tr>
<!--                                        <tr v-for="type in activity.activity_type" :key="type">-->
<!--                                            <td>Activity Type:</td>-->
<!--                                            <td>{{type}}</td>-->
<!--                                        </tr>-->
                                      <tr>
                                          <td>Followers:</td>
                                          <td>follower number</td>
                                      </tr>
                                    </table>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <div>
            <b-tabs>
                <b-tab-item label="Organisers">
                    <div class="flex">
                        <div class="table-profile"
                               v-for="organiser in organisers"
                                :key="organiser.id">
                            <ProfileSummary class="flex-item" :profile="organiser">
                                <b-button v-on:click="setProfileRole(organiser.id, 'PARTICIPANT')">
                                    Make Participant
                                </b-button>
                            </ProfileSummary>
                        </div>
                    </div>
                </b-tab-item>

                <b-tab-item label="Participants">
                    <div class="flex">
                        <div class="table-profile"
                             v-for="participant in participants"
                             :key="participant.id">
                            <ProfileSummary class="flex-item" :profile="participant">
                                <b-button v-on:click="setProfileRole(organiser.id, 'ORGANISER')">
                                    Make Organiser
                                </b-button>
                            </ProfileSummary>
                        </div>
                    </div>
                </b-tab-item>
            </b-tabs>
        </div>
    </div>
</template>

<script>
    import ProfileSummary from "../ProfileSummary";
    import router from "../../router";

    export default {
        name: "ViewActivity",
        components: {ProfileSummary},
        data() {
            return {
                activity: {},
                organisers: [],
                participants: [],
                numFollowers: 0
          }
        },
        methods: {
            getActivity() {

            },
            setProfileRole(profileId, role) {
                profileId = role;
            },
            shareActivity() {
                router.push({name:"shareActivity"})
            }
        },
        mounted() {
            //Mock data for testing; replace with appropriate API calls when implemented.
            this.activity = {
                continuous: false
            };
            this.organisers = [
                {
                    id: 1,
                    firstname: "Sample",
                    lastname: "Org",
                    activities: [],
                    gender: "Gender",
                    email: "email@cmail.dom"
                },
                {
                    id: 2,
                    firstname: "Sample",
                    lastname: "Org2",
                    activities: [],
                    gender: "Gender",
                    email: "email@cmail.dom"
                }
            ];
            for (let i = 0; i < 50; i++) {
                let participant = {
                    id: i,
                    firstname: "Tester",
                    lastname: "Participant" + i,
                    activities: [],
                    gender: "Gender",
                    email: "email@cmail.dom"
                };
                this.participants.push(participant)
            }
        }
    }
</script>

<style scoped>
    .flex {
        display: flex;
        flex-direction: row;
        flex-wrap: wrap;
        justify-content: space-between;
    }
    .flex-item {
        margin: 20px 0;
        width: 500px;
    }

</style>