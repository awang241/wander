<template>
    <div class="container containerColor">
        <!-- Header -->
        <section>
            <div id="activity-key-info">
                <div>
                    <div class="buttons" style="float:right">
                        <b-button style="float:right" @click="shareActivity"
                                  type="is-primary">
                            Share Activity
                        </b-button>
                        <b-button style="float:right" @click="editActivity"
                                  type="is-primary">
                            Edit Activity
                        </b-button>
                    </div>

                    <h1 class="title is-1">
                        {{activity.activity_name}}
                    </h1>
                    <div>
                        <h3 class="title is-5">{{privacy}}</h3>
                    </div>
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
                                <h3 class="title is-4">{{activity.activity_name}}</h3>
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
                                      <tr>
                                          <td>Followers:</td>
                                          <td>{{numFollowers}}</td>
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
    import ProfileSummary from "./ProfileSummary";
    import router from "../router";
    import api from "../Api";

    export default {
        name: "ViewActivity",
        components: {ProfileSummary},
        data() {
            return {
                activityId: this.$route.params.id,
                activity: null,
                organisers: [],
                participants: [],
                numFollowers: 0
          }
        },
        methods: {
            getRoleCounts(){
                api.getRoleCountsForActivity(this.$route.params.id, localStorage.getItem('authToken'))
                .then(response => this.numFollowers = response.data.followers)
            },
            editActivity() {
                router.push({name: 'editActivity', params: {activityProp: this.activity}});
            },
            getActivity() {
                api.getActivity(this.$route.params.id, localStorage.getItem('authToken'))
                    .then(response => this.activity = response.data)
                    .catch((error) => {
                        console.log(error)
                        router.go(-1)
                    })
            },
            setProfileRole(profileId, role) {
                profileId = role;
            },
            shareActivity() {
                router.push("/ShareActivity/" + this.activity.id + "/" + this.privacy.toLowerCase())
            },
            dateFormat(date) {
                let year = date.slice(0, 4);
                let month = date.slice(5, 7);
                let day = date.slice(8, 10);
                let hour = date.slice(11, 13);
                let min = date.slice(14, 16);
                return hour + ":" + min + " " + day + "/" + month + "/" + year;
            }
        },
        computed: {
            privacy: function () {
                switch (this.activity.privacyLevel) {
                    case 0 :
                        return "Private";
                    case 1 :
                        return "Friends";
                    case 2 :
                        return "Public";
                    default:
                        return "Private";
                }
            }
        },
        mounted() {
            //Mock data for testing; replace with appropriate API calls when implemented.
            this.getActivity()
            this.getRoleCounts()

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