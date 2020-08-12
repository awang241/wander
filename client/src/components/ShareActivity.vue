<template>
    <div class="container">
        <h1 class="title">Share Activity</h1>
        <ValidationObserver v-slot="{ handleSubmit }">
            <form @submit.prevent="handleSubmit(onShareActivityClicked)">
                <ValidationProvider rules="required" name="activityPrivacy" v-slot="{ errors, valid }" slim>
                    <b-field label="Activity Privacy"
                             :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded >
                    <template slot="label">Privacy<span>*</span></template>
                    <b-select v-model="privacy" placeholder="Choose privacy setting" expanded>
                        <option value="private">Private</option>
                        <option value="members">Members</option>
                        <option value="public">Public</option>
                    </b-select>
                    </b-field>
                </ValidationProvider>

                <div v-if="privacy == 'members'">
                    <b-field label="Emails">
                        <b-taginput
                                v-model="emails"
                                placeholder="Enter a friend's email">
                        </b-taginput>
                    </b-field>
                </div>
                <br>

                <b-button style="float: right" @click="onShareActivityClicked"
                              type="is-primary">
                        Save
                    </b-button>
                    <b-button style="float: left" @click="goBack"
                              type="is-danger">
                        Cancel
                    </b-button>
                    <br>
            </form>
        </ValidationObserver>
    </div>
</template>


<script>
    import store from "../store";
    import router from "../router";
    import toastMixin from "../mixins/toastMixin";
    import {ValidationObserver, ValidationProvider} from "vee-validate";
    import ActivityShareConfirmation from "./ActivityShareConfirmation";
    import Api from "../Api";


    export default {
        name: "ShareActivity",
        prop: ['activityPrivacy'],
        mixins: [toastMixin],
        components: {
            ValidationProvider,
            ValidationObserver
        },
        data() {
            return {
                privacy: 'private',
                originalPrivacy: "public",
                emails: {},
                activityId: this.$route.params.id,
            }
        },
        mounted() {
            this.checkAuthenticationStatus()
        },
        methods: {
            onShareActivityClicked() {
                if(this.isPrivacyMoreRestrictive()){
                    this.$buefy.modal.open({
                        parent: this,
                        component: ActivityShareConfirmation,
                        trapFocus: true,
                        scroll: "clip"
                    })
                }
                Api.editActivityPrivacy(store.getters.getUserId, this.$route.params.id, this.privacy, localStorage.getItem('authToken'))
                    .then((response) => {
                        console.log(response);
                        this.successToast("Activity privacy updated")
                        router.go(-1)
                    })
                    .catch(error => console.log(error));
            },
            isPrivacyMoreRestrictive(){
                const privacyDict = {"public": 1, "members": 2, "private": 3}
                return privacyDict[this.privacy] > privacyDict[this.originalPrivacy]
            },
            goBack() {
                router.go(-1)
            },

            checkAuthenticationStatus() {
                if (!store.getters.getAuthenticationStatus) {
                    router.push({path: '/'})
                }
            },
        }
    }
</script>


<style scoped>
    .container {
        width: 800px;
    }

    @media only screen and (max-width: 600px) {
        .container {
            width: 100%;
        }
    }

    span {
        color: red;
    }

</style>
