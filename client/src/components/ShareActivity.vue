<template>
    <div class="container">
        <h1 class="title">Share Activity</h1>
        <ValidationObserver v-slot="{ handleSubmit }">
            <form @submit.prevent="handleSubmit(shareActivity)">
                <ValidationProvider rules="required" name="activityPrivacy" v-slot="{ errors, valid }" slim>
                    <b-field label="Activity Privacy"
                             :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded >
                    <template slot="label">Privacy<span>*</span></template>
                    <b-select v-model="privacy" placeholder="Choose privacy setting" expanded>
                        <option value="0">Private</option>
                        <option value="1">Friends</option>
                        <option value="2">Public</option>
                    </b-select>
                    </b-field>
                </ValidationProvider>

                <b-field label="Emails">
                    <b-taginput
                            v-model="emails"
                            :data="allEmails"
                            autocomplete
                            @typing="getFilteredEmails"
                            :open-on-focus="false"
                            placeholder="Enter a friend's email">
                    </b-taginput>
                </b-field>


                <b-button style="float: right" @click="shareActivity"
                              type="is-primary">
                        Share
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


    export default {
        name: "AddActivity",
        mixins: [toastMixin],
        components: {
            ValidationProvider,
            ValidationObserver
        },
        // data() {
        //     privacy
        // },
        mounted() {
            this.checkAuthenticationStatus()
            console.log(this.$props.activityProp)
        },
        methods: {

            shareActivity() {

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
