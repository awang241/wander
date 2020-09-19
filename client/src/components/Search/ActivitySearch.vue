<template>
    <div class="container">
        <h1 class="title">Activity Search</h1>
        <b-field group-multiline grouped>
            <b-field label="Enter a location" expanded>
                <AutoCompleteLocation v-on:locationStringChanged="updateMapLocationFromAutoComplete" v-on:updateMap="updateLocation" v-bind:profileLocation="this.profile.location" ref="autocomplete"></AutoCompleteLocation>
            </b-field>
            <b-field label="Max distance (km)">
                <b-numberinput v-model="maxDistance" type="is-primary" :min="1" :max="200"></b-numberinput>
            </b-field>
        </b-field>
        <ActivityTypesField v-on:updateSearchMethod="newSearchMethod => activitySearchType = newSearchMethod"
                            v-on:updateChosenActivityTypes="newActivityTypes => chosenActivityTypes = newActivityTypes"
                            :chosenActivityTypes="chosenActivityTypes"
                            :activitySearchType="activitySearchType"></ActivityTypesField>
        <br>
        <MapPane marker-label="Profile Location" :location-choice-coordinates="profileLocationLatLong" v-bind:address="this.profile.location.address"
                 v-on:locationChoiceChanged="updateLocation" :info-window-content="this.informationWindowData"></MapPane>
        <br>

        <div class="row">
            <br>
            <b-field style="float:left">
                <b-button type="is-danger" @click="clearLocation()">Clear</b-button>
            </b-field>
            <b-field style="float:right">
                <b-button type="is-primary" @click="search()">Search</b-button>
            </b-field>
            <br>
        </div>
        <br/>
    </div>
</template>

<script>
    import googleMapsInit from '../../utils/googlemaps'
    import MapPane from "../Reusables/MapPane";
    import ActivityTypesField from "../Activities/ActivityHelpers/ActivityTypesField";
    import Api from "../../Api";
    import store from "../../store";
    import toastMixin from "../..//mixins/toastMixin";
    import AutoCompleteLocation from "../Reusables/AutoCompleteLocation";

    export default {
        name: "ActivitySearch",
        components: {
            MapPane, ActivityTypesField, AutoCompleteLocation,
        },
        mixins: [toastMixin],
        data() {
            return {
                geocoder: null,
                profile: {},
                maxDistance: 50,
                activitySearchType: "all",
                chosenActivityTypes: [],
                activityResults: [],
                store: store,
                profileLocationLatLong: null,
                locationString: ""
            }
        },
        methods: {
            clearLocation() {
                // also clear autocomplete field when merged in
                this.maxDistance = 50;
                this.chosenActivityTypes = []
            },
            search() {
                const searchParameters = this.getSearchParameters();
                Api.getActivitiesByLocation(localStorage.getItem('authToken'), searchParameters).then(response => {
                    this.activityResults = response.data.results
                })
            },
            getSearchParameters() {
                const searchParameters = {};
                searchParameters.distance = this.maxDistance
                searchParameters.latitude = this.profileLocationLatLong.lat
                searchParameters.longitude = this.profileLocationLatLong.lng
                if (this.chosenActivityTypes.length > 0) {
                    searchParameters.activityTypes = this.chosenActivityTypes.join(",")
                    searchParameters.searchMethod = this.activitySearchType
                }
                return searchParameters
            },

            setDefaultProfileLocation() {
                Api.getProfile(this.store.getters.getUserId, localStorage.getItem('authToken'))
                    .then((response) => {
                        this.profile = response.data;
                        if (this.profile.location) {
                            this.profileLocationLatLong = {
                                lat: this.profile.location.latitude,
                                lng: this.profile.location.longitude
                            };
                        }
                    })
                    .catch(() => {
                        this.warningToast("Error occurred while getting your location details.");
                    })
            },
            updateLocation(location) {
                this.geocoder.geocode({'location': {lat: location.lat(), lng: location.lng()}}, (results, status) => {
                    if (status === 'OK') {
                        document.getElementById("autocompleteLocation").value = results[0].formatted_address
                        this.locationString = results[0].formatted_address
                        this.profileLocationLatLong = {lat: location.lat(), lng: location.lng()}

                    }
                })
            },
            updateMapLocationFromAutoComplete(location) {
                this.profileLocationLatLong = {lat: location.latitude, lng: location.longitude}
                this.locationString = location.address
            },

        },
        async mounted() {
            this.google = await googleMapsInit();
            this.setDefaultProfileLocation();
            this.geocoder = new this.google.maps.Geocoder;
        }
    }

</script>

<style scoped>
    .container {
        margin-top: 0px;
    }

</style>