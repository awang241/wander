<template>
    <div class="container">
        <h1 class="title">Activity</h1>
        <ValidationObserver v-slot="{ handleSubmit }">

            <form @submit.prevent="handleSubmit(createActivity)">
                <ValidationProvider rules="required|minName" name="Activity Name" v-slot="{ errors, valid }" slim>
                    <b-field label="Activity Name"
                             :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded>
                        <template slot="label">Activity Name <span class="requiredStar">*</span></template>
                        <b-input v-model="activity.name" placeholder="Enter activity name here"></b-input>
                    </b-field>
                </ValidationProvider>

                <ValidationProvider rules="required|activityDescription" name="Description" v-slot="{ errors, valid }" slim>
                    <b-field label="Description"
                             :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded>
                        <template slot="label">Description <span class="requiredStar">*</span></template>
                        <b-input v-model="activity.description" maxlength="500" type="textarea"
                                 placeholder="Enter a description"></b-input>
                    </b-field>
                </ValidationProvider>

                <div class="block">

                    <b-field > <template slot="label">Activity Duration <span class="requiredStar">*</span></template></b-field>
                    <b-radio v-model="activity.activityDuration"
                             name="name"
                             native-value="Continuous">
                        Continuous
                    </b-radio>
                    <b-radio v-model="activity.activityDuration"
                             name="name"
                             native-value="Duration">
                        Duration
                    </b-radio>
                </div>


                <div v-if="!isContinuous">
                    <b-field group-multiline grouped>
                        <b-field label="Start date" expanded>
                            <input class="input" type="date" v-model="activity.startDate">
                        </b-field>
                        <b-field label="End date" expanded>
                            <input class="input" type="date" v-model="activity.endDate">
                        </b-field>
                    </b-field>
                    <b-field group-multiline grouped>
                        <b-field label="Start time" expanded>
                            <input class="input" type="time" v-model="activity.startTime">
                        </b-field>
                        <b-field label="End time" expanded>
                            <input class="input" type="time" v-model="activity.endTime">
                        </b-field>
                    </b-field>
                    <br>
                </div>
                <b-field label="Enter a location" expanded>
                    <AutoCompleteLocation v-on:locationStringChanged="updateMapLocationFromAutoComplete" v-on:updateMap="updateLocation" ref="autocomplete" v-bind:profileLocation="locationObject"></AutoCompleteLocation>
                </b-field>
                <MapPane marker-label="Activity Location" :location-choice-coordinates="activityLocationLatLong" v-on:locationChoiceChanged="updateLocation" ref="mapPaneRef" :default_width="720" :default_height="500"></MapPane>
              <br>
              <br>
              <h4 class="label">Add at least one activity type <span class="requiredStar">*</span></h4>
                <b-field>
                    <b-select placeholder="Select at least one activity type" v-model="newActivityType" expanded>
                        <option
                                v-for="activityType in possibleActivityTypes"
                                :value="activityType"
                                :key="activityType">
                            {{ activityType }}
                        </option>
                    </b-select>
                    <b-button type="is-primary" @click="addActivityType">Add</b-button>
                </b-field>
                <List v-bind:chosenItems="activity.chosenActivityTypes" v-on:deleteListItem="deleteActivityType"></List>
                <br>
                <div class="column">
                    <div class="is-pulled-left">
                        <b-button type="is-danger" @click="goBack">Cancel</b-button>
                    </div>
                    <div class="is-pulled-right">
                        <b-field>
                            <b-button native-type="submit" class="is-primary">Submit</b-button>
                        </b-field>
                    </div>
                    <br>
                </div>
            </form>
        </ValidationObserver>
    </div>

</template>


<script>
    import List from "../Misc/HelperComponents/List";
    import Api from "../../Api"
    import store from "../../store";
    import router from "../../router";
    import toastMixin from "../../mixins/toastMixin";
    import {ValidationProvider, ValidationObserver} from 'vee-validate'
    import dateTimeMixin from "../../mixins/dateTimeMixin";
    import MapPane from "../Location/MapPane";
    import AutoCompleteLocation  from "../Location/AutoCompleteLocation";
    import googleMapsInit from "../../utils/googlemaps";


    export default {
        name: "AddActivity",
        components: {
            MapPane,
            List,
            AutoCompleteLocation,
            ValidationProvider,
            ValidationObserver
        },
        mixins: [toastMixin, dateTimeMixin],
        props: {
            //Activity the user is editing if one exists
            activityProp: {
                type: Object,
                //If user is creating an activity rather than editing a default object is given
                default: function () {
                    return {
                        name: "",
                        description: "",
                        startDate: null,
                        activityDuration: "Continuous",
                        chosenActivityTypes: [],
                        endDate: null,
                        startTime: "",
                        endTime: "",
                        location: "",
                        continuous: true,
                        creating: true,
                        latitude: null,
                        longitude: null
                    }
                }
            }
        },

        computed: {
            // a computed getter as radio buttons cannot return boolean values
            isContinuous: function () {
                return this.activity.activityDuration === "Continuous"
            },

            combinedStartDate: function () {
                return this.combineDateAndTime(this.activity.startDate, this.activity.startTime)
            },
            combinedEndDate: function () {
                return this.combineDateAndTime(this.activity.endDate, this.activity.endTime)
            }
        },
        data() {
            return {
                activity: {},
                newActivityType: "",
                possibleActivityTypes: [],
                //The location of the activity in lat and long (will be displayed on map if it exists)
                activityLocationLatLong: null,
                google: null,
                geocoder: null,
                locationString: "",
                locationObject: {
                  address: "",
                  latitude: null,
                  longitude: null
                }
            }
        },
        async mounted() {
            if(this.activityProp.location !== "") {
              this.getLocation()
            }
            this.checkAuthenticationStatus()
            this.getPossibleActivityTypes()
            this.activity = this.convertToProp(this.$props.activityProp)

            this.google = await googleMapsInit();
            this.geocoder = new this.google.maps.Geocoder;
            this.updateLocationString();
        },
        methods: {
          getLocation() {
            this.locationObject = {address: this.activityProp.location, latitude: this.activityProp.latitude, longitude: this.activityProp.longitude}
          },
          //Updates the users location coordinates with the location on the map they have clicked
            updateLocation(location){
              this.geocoder.geocode({'location': {lat: location.lat(), lng: location.lng()}}, (results, status) => {
                if (status === 'OK') {
                  document.getElementById("autocompleteLocation").value = results[0].formatted_address
                  this.locationString = results[0].formatted_address
                  this.activity.location = results[0].formatted_address
                  this.activity.latitude = location.lat()
                  this.activity.longitude = location.lng()
                  this.activityLocationLatLong = {lat: location.lat(), lng: location.lng()}
                  this.$refs.mapPaneRef.setZoomLevel(this.locationString)
                }
              })
            },
            goBack() {
                router.go(-1)
            },
            getPossibleActivityTypes() {
                Api.getActivityTypesList()
                    .then(response => this.possibleActivityTypes = response.data.allActivityTypes)
                    .catch(error => this.successToast(error))
            },
            addActivityType() {
                if (this.newActivityType === "") {
                    this.warningToast("No Activity type selected")
                } else if (this.activity.chosenActivityTypes.includes(this.newActivityType)) {
                    this.warningToast("Activity type already in list")
                } else {
                    this.activity.chosenActivityTypes = [...this.activity.chosenActivityTypes, this.newActivityType]
                }
            },
            dateFormatter(dt) {
                return dt.toLocaleDateString('en-GB', {year: 'numeric', month: 'numeric', day: 'numeric'});
            },
            deleteActivityType(typeToDelete) {
                this.activity.chosenActivityTypes = this.activity.chosenActivityTypes.filter(type => type != typeToDelete)
            },
            validateActivity() {
                let isValid = true;
                if (this.activity.chosenActivityTypes.length < 1) {
                    this.warningToast("You must choose at least one activity type")
                    isValid = false
                } else if (!this.isContinuous) {
                    this.continuous = false
                    const startDate = Date.parse(this.combinedStartDate)
                    const endDate = Date.parse(this.combinedEndDate)
                    if (isNaN(startDate) || isNaN(endDate)) {
                        this.warningToast("Invalid dates entered!")
                        isValid = false
                    } else if (Date.parse(this.combinedStartDate) > Date.parse(this.combinedEndDate)) {
                        this.warningToast("The end date must be after the start date")
                        isValid = false
                    }
                }
                return isValid
            },
            createActivity() {
                if (this.validateActivity()) {
                    let activity = {
                        "activity_name": this.activity.name,
                        "description": this.activity.description,
                        "activity_type": this.activity.chosenActivityTypes,
                        "continuous": this.isContinuous,
                        "location": this.locationString,
                        "latitude": this.activityLocationLatLong.lat,
                        "longitude": this.activityLocationLatLong.lng
                    }
                    if (!this.isContinuous) {
                        activity.start_time = this.combinedStartDate
                        activity.end_time = this.combinedEndDate
                    }

                    this.submitActivity(activity)
                }
            },
            submitActivity(activity) {
                const originalActivity = this.convertToProp(this.activityProp)
                if (this.activity.creating) {
                    Api.createActivity(store.getters.getUserId, activity, localStorage.getItem('authToken'))
                        .then(() => {
                            this.successToast("Activity created")
                            router.push({path: '/Activities'})
                        })
                } else {
                    if (JSON.stringify(this.activity) === JSON.stringify(originalActivity)) {
                        this.warningToast("No changes made")
                    } else {
                        Api.updateActivity(store.getters.getUserId, localStorage.getItem('authToken'), activity, this.activityProp.id)
                            .then(() => {
                                this.successToast("Activity updated")
                                router.push({path: '/Activities'})
                            })
                    }
                }
            },
            checkAuthenticationStatus() {
                if (!store.getters.getAuthenticationStatus) {
                    router.push({path: '/'})
                }
            },
            //Component to create/edit activities needs a prop with a different format to the HTTP GET request
            //This method converts the data from the request to a usable prop for the create/edit component
            convertToProp(activity) {
                //If we are creating activity rather than editing we don't need to modify props
                if (activity.creating) {
                    return activity
                }
                let activityProp = {
                    "name": activity.activity_name,
                    "description": activity.description,
                    "location": activity.location,
                    "chosenActivityTypes": activity.activity_type,
                    "id": activity.id,
                    "latitude": activity.latitude,
                    "longitude": activity.longitude
                }
                if (activity.continuous) {
                    activityProp.activityDuration = "Continuous"
                    activityProp.startDate = null
                    activityProp.endDate = null
                    activityProp.startTime = ""
                    activityProp.endTime = ""
                } else {
                    activityProp.activityDuration = "Duration"
                    //Converting the UTC format to format used by HTML date inputs. Surely a better way to do this
                    activityProp.startDate = activity.start_time.slice(0, 10)
                    activityProp.startTime = activity.start_time.slice(11, 16)
                    activityProp.endDate = activity.end_time.slice(0, 10)
                    activityProp.endTime = activity.end_time.slice(11, 16)
                }

                return activityProp
            },
          updateLocationString() {
            if (this.activity.location.address) {
              this.locationString = this.activity.location.address
            }
          },
          updateMapLocationFromAutoComplete(location) {
            this.activityLocationLatLong = {lat: location.latitude, lng: location.longitude}
            this.activity.location = location.address
            this.activity.latitude = location.latitude
            this.activity.longitude = location.longitude
            this.locationString = location.address;
            this.$refs.mapPaneRef.setZoomLevel(this.locationString)
          }
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

    .column {
        padding: 0;
        margin: 0;
    }

    .requiredStar {
        color:red
    }

</style>
