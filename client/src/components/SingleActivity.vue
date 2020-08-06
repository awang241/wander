<template>
    <div class="column">
        <div class="card">
            <div class="card-content">
                <h3 class="title is-4">{{activity.activity_name}}</h3>
                Role: CREATOR
                <b-button style="float: right" @click="activityDetail(activity)"
                          type="is-text">
                    View more
                </b-button>
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
                    <b-button style="float: left" @click="deleteActivity(activity.id)"
                              type="is-danger">
                        Delete
                    </b-button>
                    <b-button style="float: right" @click="editActivity(activity)"
                              type="is-primary">
                        Edit
                    </b-button>
                </div>
                <br>
            </div>
        </div>
    </div>
</template>

<script>
    import Api from '../Api';
    import router from "../router";
    import store from "../store"
    import toastMixin from "../mixins/toastMixin";

    export default {
        name: "SingleActivity",
        props: ["activity"],
        mixins: [toastMixin],
        data() {
            return {
                store: store,
            }
        },
        methods: {
            goToAddActivity() {
                router.push({path: '/AddActivity'});
            }, editActivity(activity) {
                router.push({name: 'editActivity', params: {activityProp: activity}})
            }, activityDetail(activity) {
                router.push({path: 'Activities/' + activity.id})
            },
            deleteActivity(id) {
                Api.deleteActivity(store.getters.getUserId, localStorage.getItem('authToken'), id)
                    .then((response) => {
                        console.log(response);
                        this.warningToast("Activity deleted")
                        this.activities = this.activities.filter(activity => activity.id != id);
                    })
                    .catch(error => console.log(error));
            },
            dateFormat(date) {
                let year = date.slice(0, 4);
                let month = date.slice(5, 7);
                let day = date.slice(8, 10);
                let hour = date.slice(11, 13);
                let min = date.slice(14, 16);
                return hour + ":" + min + " " + day + "/" + month + "/" + year;
            }
        }
    }

</script>

<style scoped>

    .containerColor {
        background-color: #F7F8F9
    }

    #activities-key-info {
        display: flex;
        justify-content: space-between;
        padding: 0 1rem;
    }

</style>