<template>
    <h1>wip</h1>
<!--    <div class="container containerColor">-->
<!--        &lt;!&ndash; Header &ndash;&gt;-->
<!--        <section>-->
<!--            <div id="activity-key-info">-->
<!--                <div>-->
<!--                    <h1 class="title is-1">-->
<!--                        {{ activity.activity_name }}-->
<!--                    </h1>-->
<!--                    <h2 class="subtitle is-5">-->
<!--                        ({{ activityMembership.creator }})-->
<!--                    </h2>-->
<!--                    <div>-->
<!--                        <h3 class="title is-5"> Privacy:{{ activity.privacy }}</h3>-->
<!--                    </div>-->
<!--                </div>-->
<!--                <div>-->
<!--                    <b-button v-if="creator/organiser"-->
<!--                              @click="editActivity"-->
<!--                              type="is-primary">-->
<!--                        Edit-->
<!--                    </b-button>-->
<!--                </div>-->
<!--            </div>-->
<!--        </section>-->

<!--        <div class="has-same-height is-gapless">-->
<!--            <div v-if="activities.length">-->
<!--                <div>-->
<!--                    <div class="column">-->
<!--                        &lt;!&ndash; Activities &ndash;&gt;-->
<!--                        <div class="card">-->
<!--                            <div class="card-content">-->
<!--                                <h3 class="title is-4">{{activity.activity_name}}</h3>-->
<!--                                <div class="content">-->
<!--                                    <table class="table-profile">-->
<!--                                        <caption hidden>Displayed Activity Table</caption>-->
<!--                                        <tr>-->
<!--                                            <th colspan="1" scope="col"></th>-->
<!--                                            <th colspan="2" scope="col"></th>-->
<!--                                        </tr>-->
<!--                                        <tr>-->
<!--                                            <td>Description:</td>-->
<!--                                            <td>{{activity.description}}</td>-->
<!--                                        </tr>-->
<!--                                        <tr>-->
<!--                                            <td>Continous/Duration:</td>-->
<!--                                            <td v-if="activity.continuous">continuous</td>-->
<!--                                            <td v-else>duration</td>-->
<!--                                        </tr>-->

<!--                                        <tr v-if="!activity.continuous">-->
<!--                                            <td>Start Time:</td>-->
<!--                                            <td>UTC {{dateFormat(activity.start_time)}}</td>-->
<!--                                        </tr>-->
<!--                                        <tr v-if="!activity.continuous">-->
<!--                                            <td>End Time:</td>-->
<!--                                            <td>UTC {{dateFormat(activity.end_time)}}</td>-->
<!--                                        </tr>-->

<!--                                        <tr>-->
<!--                                            <td>Location:</td>-->
<!--                                            <td>{{activity.location}}</td>-->
<!--                                        </tr>-->
<!--                                        <tr v-for="type in activity.activity_type" :key="type">-->
<!--                                            <td>Activity Type:</td>-->
<!--                                            <td>{{type}}</td>-->
<!--                                        </tr>-->
<!--                                    </table>>-->
<!--                                </div>-->
<!--                            </div>-->
<!--                        </div>-->
<!--                    </div>-->
<!--                </div>-->
<!--            </div>-->
<!--        </div>-->
<!--    </div>-->
</template>

<script>
    export default {
        name: "ViewActivity",
        methods: {
            getActivity() {

            }
        }
    }
</script>

<style scoped>

</style>