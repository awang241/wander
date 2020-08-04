<template>
    <div v-if="store.getters.getAuthenticationStatus" class="container containerColor">
        <!-- Header -->
        <section>
            <br>
            <div id="activities-key-info">
                <div>
                    <h1 class="title is-1">
                        Activities
                    </h1>
                </div>
                <!-- redirect to add activity -->
                <div>
                    <b-button v-if="store.getters.getAuthenticationLevel > 0" @click="goToAddActivity"
                              type="is-primary">
                        Add Activity
                    </b-button>
                </div>
            </div>
        </section>
        <br>
        <br>
        <div class="has-same-height is-gapless">
            <div class="tabs is-centered">

                <ul>
                    <li><a v-on:click="changeToMyActivities">My Activities</a></li>
                    <li><a v-on:click="changeToParticipatingActivities">Participating</a></li>
                    <li><a v-on:click="changeToFollowingActivities">Following</a></li>
                    <li><a v-on:click="changeToDiscoverActivities">Discover Activities</a></li>

                </ul>
            </div>
        </div>
        <div>
            <component v-bind:is="component"/>
        </div>
    </div>
</template>

<script>
    // import api from '../Api';
    import discoverActivities from "./ActivityColumns/DiscoverActivities";
    import myActivities from "./ActivityColumns/MyActivities";
    import followingActivities from "./ActivityColumns/FollowingActivities";
    import participatingActivities from "./ActivityColumns/ParticipatingActivities";
    import store from "../store"
    import router from "../router";

    export default {
        name: "Activities",
        data() {
            return {
                activities: null,
                store: store,
                component: "",
            }
        },
        methods: {
            changeToDiscoverActivities() {
                this.component = discoverActivities;
            },
            changeToMyActivities() {
                this.component = myActivities;
            },
            changeToFollowingActivities() {
                this.component = followingActivities;
            },
            changeToParticipatingActivities() {
                this.component = participatingActivities;
            },
            goToAddActivity() {
                router.push({path: '/AddActivity'});
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