<template>
    <div class="card">
        <div class="columns">
            <div class="column">
                <h4><strong>{{name}}</strong></h4>
                <p>{{profile.gender}}</p>
                <p>{{profile.email}}</p>
            </div>
            <div v-if="profile.activities.length > 0" class="column">
                <strong>Activity Types</strong>
                <div v-for="activity in profile.activities" :key="activity">
                    <p> {{activity}}</p>
                </div>
            </div>
            <b-button v-if="false" type="is-text" @click="gotoProfile(profile.id)">View profile</b-button>
            <b-menu-item v-else class="is-vertical-center">
                <template slot="label">
                    <b-dropdown aria-role="list" class="is-pulled-right" position="is-bottom-left">
                        <b-icon icon="ellipsis-v" slot="trigger"></b-icon>
                        <b-dropdown-item aria-role="listitem" @click="gotoProfile">View profile</b-dropdown-item>
                        <b-dropdown-item aria-role="listitem" @click="editProfile">Edit profile </b-dropdown-item>
                        <b-dropdown-item aria-role="listitem" @click="deleteProfile">Delete profile</b-dropdown-item>
                        <b-dropdown-item aria-role="listitem">Make admin</b-dropdown-item>
                    </b-dropdown>
                </template>
            </b-menu-item>
        </div>
    </div>
</template>

<script>
    import Profile from "./Profile.vue";
    import EditProfile from "./editprofile/EditProfile"

    export default {
        name: "ProfileSummary",
        data() {
            return {
                profileData: {}
            }
        },
        mounted() {
            this.profileData = this.props.profile;
        },
        computed: {
            name() {
                return `${this.profile.firstname}  ${this.profile.lastname}`
            }
        },
        props: {
            profile: {
                type: Object,
                required: true
            }
        },
        methods: {
            gotoProfile() {
                this.$buefy.modal.open({
                    parent: this,
                    props: {id: this.profile.id},
                    component: Profile,
                    trapFocus: true,
                    scroll: "clip"
                })
            },
            editProfile(){
                this.$buefy.modal.open({
                    parent: this,
                    props: {id: this.profile.id},
                    component: EditProfile,
                    trapFocus: true,
                    scroll: "clip"
                })
            },
            deleteProfile(){
                console.log("deleted profile!")
            }
        }
    }
</script>

<style scoped>
    .columns {
        padding: 1rem;
    }
    li{
        list-style-type: none;
    }
    .is-vertical-center {
        display: flex;
        align-items: center;
        padding-right: 1rem;
    }

</style>